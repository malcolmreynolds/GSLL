;********************************************************
; file:        interface.lisp                            
; description: Macros to interface GSL functions.
; date:        Mon Mar  6 2006 - 22:35                   
; author:      Liam M. Healy
; modified:    Sun Jun  4 2006 - 21:39
;********************************************************

(in-package :gsl)

(export '(gsl-lookup *make-sequence-type*))

;;;;****************************************************************************
;;;; Lookup table to find CL functions from C function names
;;;;****************************************************************************

(defparameter *sf-name-mapping* nil
  "An alist mapping the CL and GSL names of the special functions.
   This will only be populated when macros are expanded.")

(defun map-name (cl-name gsl-name)
  (setf *sf-name-mapping* (acons cl-name gsl-name *sf-name-mapping*)))

(defun gsl-lookup (string)
  "Find the CL function name given the GSL C special function function name.
   If cl-ppcre (http://www.weitz.de/cl-ppcre/) is installed, the string
   can be a regular expression; otherwise, it must match the C name exactly
   except for case."
  (remove string *sf-name-mapping*
	  :key #'rest
	  :test-not
	  (if (find-package :ppcre)
	      (symbol-function (intern "ALL-MATCHES" :ppcre))
	      #'string-equal)))

;;;;****************************************************************************
;;;; Symbol-type declaration
;;;;****************************************************************************

;;; An "st" or symbol-type is a list (symbol type) where
;;; type could be (element-type array-dim).  These are examples of lists
;;; of sts: 
 ;; ((#:RET3500 SF-RESULT))
 ;; ((#:RET3501 (:DOUBLE (- NMAX NMIN)))) 
 ;; ((#:RET3502 (:DOUBLE (1+ KMAX))) (#:RET3503 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3504 (:DOUBLE (1+ KMAX))) (#:RET3505 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3506 :DOUBLE) (#:RET3507 :DOUBLE))

(defun st-symbol (decl)
  (first decl))

(defun st-type (decl)
  (second decl))

(defun st-arrayp (decl)
  (listp (st-type decl)))

(defun st-array-pointer-last-p (decl)
  (listp (st-type decl)))

(defun st-eltype (decl)
  (first (st-type decl)))

(defun st-dim (decl)
  (second (st-type decl)))

(defun st-pointer-last-p (decl)
  (third (st-type decl)))

(defun wfo-declare (d)
  `(,(st-symbol d)
    ,@(if (st-arrayp d)
	  `(',(st-eltype d) ,(st-dim d))
	  `(',(st-type d)))))

;;;;****************************************************************************
;;;; Checking results from GSL functions
;;;;****************************************************************************

(defun check-gsl-status (status-code context)
  "Check the return status code from a GSL function and signal a warning
   if it is not :SUCCESS."
  (unless (eql :success (cffi:foreign-enum-keyword 'gsl-errorno status-code))
    (warn 'gsl-warning
	  :gsl-errno status-code :gsl-context context)))

(defun check-null-pointer (pointer error-code reason)
  (when (cffi:null-pointer-p pointer)
    (error 'gsl-error
	   :gsl-errno (cffi:foreign-enum-value 'gsl-errorno error-code)
	   :gsl-reason reason)))

(defun check-null-pointers (check-null-pointers creturn)
  (let ((cret (find :creturn check-null-pointers :key #'first)))
    (when cret
	`((check-null-pointer ,creturn ,@(rest cret))))))

(defun success-failure (value)
  "If status indicates failure, return NIL, othewise return T."
  (not (eql value (cffi:foreign-enum-value 'gsl-errorno :FAILURE))))

;;;;****************************************************************************
;;;; Macro defun-gsl 
;;;;****************************************************************************

(defvar *special-c-return* '(:error-code :number-of-answers :success-failure))

;;; c-arguments List of (symbol c-type). Anything not in arglist will be allocated.
;;; arglist    List of CL arguments.
;;; c-return,  a symbol naming a type, (e.g. :int, :double, :void),
;;;            or a list of (symbol type) to name the value,
;;;            or :error-code, :number-of-answers, :success-failure. 
;;; return, a list of quantities to return.
;;;            May be or include :c-return to include the c-return value
;;;            or its derivatives.
;;;            Default are allocated quantities in c-arguments, or :c-return if none.
(defmacro defun-gsl
    (name arglist gsl-name c-arguments
     &key (c-return :error-code)
     (return nil return-supplied-p)
     (type :function) index (export (not (eq type :method)))
     documentation
     check-null-pointers invalidate after)
  (let* ((cargs (substitute '(mode sf-mode) :mode c-arguments))
	 (carg-symbs
	  (remove-if-not #'symbolp
			 (mapcar #'st-symbol (remove :mode c-arguments))))
	 (clargs
	  (or arglist carg-symbs))
	 (arglist-symbs
	  (when arglist			; can be method, so get symbol
	    (mapcar (lambda (x) (if (listp x) (first x) x)) arglist)))
	 (cret-type (if (member c-return *special-c-return*)
			:int
			(if (listp c-return) (st-type c-return) c-return)))
	 (cret-name
	  (if (listp c-return) (st-symbol c-return) (make-symbol "CRETURN")))
	 (allocated		     ; Foreign objects to be allocated
	  (remove-if (lambda (s) (member s arglist-symbs)) carg-symbs))
	 (allocated-decl
	  (mapcar
	   (lambda (s) (find s cargs :key #'st-symbol))
	   allocated))
	 (clret (or (substitute cret-name :c-return return)
		    (mapcan #'cl-convert-form allocated-decl)
		    invalidate
		    (unless (eq c-return :void)
		      (list cret-name)))))
    `(progn
       (,(if (eq type :function) 'defun 'defmethod)
	 ,name
	 ,(if (member :mode c-arguments)
	      `(,@clargs &optional (mode :double-prec))
	      `(,@clargs))
	 ,@(when documentation (list documentation))
	 (,@(if allocated
		`(cffi:with-foreign-objects
		     ,(mapcar #'wfo-declare allocated-decl))
		'(let ()))
	    (let ((,cret-name
		   (cffi:foreign-funcall
		    ,gsl-name
		    ,@(mapcan
		       (lambda (arg)
			 (list (if (member (st-symbol arg) allocated)
				   :pointer
				   (st-type arg))
			       (st-symbol arg)))
		       cargs)
		    ,cret-type)))
	      ,@(case c-return
		      (:void `((declare (ignore ,cret-name))))
		      (:error-code	; fill in arguments
		       `((check-gsl-status ,cret-name 'bah))))
	      ,@(when invalidate `((cl-invalidate ,@invalidate)))
	      ,@(check-null-pointers check-null-pointers cret-name)
	      ,@after
	      (values
	       ,@(case c-return
		       (:number-of-answers
			(mapcan
			 (lambda (vbl seq)
			   `((when (> ,cret-name ,seq) ,vbl)))
			 clret
			 (loop for i below (length clret) collect i)))
		       (:success-failure
			(if (equal clret invalidate)
			    ;; success-failure more important than passed-in
			    `((success-failure ,cret-name) ,@clret)
			    `(,@clret (success-failure ,cret-name))))
		       (t (unless (and (null return) return-supplied-p)
			    clret)))))))
       (map-name ',(or index name) ,gsl-name)
       ,@(when export `((export ',name))))))


;;;;****************************************************************************
;;;; examples
;;;;****************************************************************************

#+development
(t
 (rearrange-sf-result-err
  (mapcan #'cl-convert-form return-symb-type)))
;;(x &optional (mode :double-prec))

;;;; Ports

#|
(defun-gsl coulomb-wave-F-array (array l-min kmax eta x)
  "gsl_sf_coulomb_wave_F_array"
  ((L-min :double) (kmax :int) (eta :double) (x :double)
   ((pointer array) gsl-vector-c) (exponent :double))
  :documentation
  "The Coulomb wave function @math{F_L(\eta,x)} for
@math{L = Lmin \dots Lmin + kmax}, storing the results in @var{array}.
In the case of overflow the exponent is stored in @var{exponent}."
  :return (array exponent))

(defun-gsl airy-Ai-new (x)  
  "gsl_sf_airy_Ai_e"			; gsl-name
  ((x :double) :mode (result sf-result))	; c-arguments
  :documentation "The Airy function Ai(x).")

(defun-gsl solve-quadratic-new (a b c)
  "gsl_poly_solve_quadratic"
  ((a :double) (b :double) (c :double) (root1 :double) (root2 :double))
  :documentation
  "The real roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if the roots are not real, these
   values are NIL."
  :c-return :number-of-answers)

(defun-gsl permutation-next-new (p)
  "gsl_permutation_next" ((p gsl-permutation-c))
  :c-return :success-failure
  :invalidate (p)
  :return (p)
  :documentation
  "Advance the permutation @var{p} to the next permutation
   in lexicographic order and return T.  If no further
   permutations are available, return NIL and leave
   @var{p} unmodified.  Starting with the identity permutation and
   repeatedly applying this function will iterate through all possible
   permutations of a given order.")
|#
