;********************************************************
; file:        interface.lisp                            
; description: Macros to interface GSL functions.
; date:        Mon Mar  6 2006 - 22:35                   
; author:      Liam M. Healy
; modified:    Tue May 30 2006 - 22:34
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
;;;;  Native types
;;;;****************************************************************************

(cffi:defcstruct gsl-complex
  "A complex number in GSL."
  (dat :double :count 2))

(defun complex-to-cl (gsl-complex &optional (index 0))
  "Make a CL complex number from the GSL pointer to a complex struct or
   an array of complex structs and an index into the array." 
  (let ((carr (cffi:foreign-slot-value
	       (cffi:inc-pointer
		gsl-complex
		(* index (cffi:foreign-type-size 'gsl-complex)))
	       'gsl-complex 'dat)))
    (complex (mem-aref carr :double 0)
	     (mem-aref carr :double 1))))

(defun double-to-cl (double &optional (index 0))
  (cffi:mem-aref double :double index))

(defun size-to-cl (size &optional (index 0))
  (cffi:mem-aref size :size index))

(defun cl-convert-function (type)
  (case type
    (:double 'double-to-cl)
    (:size 'size-to-cl)
    (gsl-complex 'complex-to-cl)))

(defparameter *make-sequence-type* 'list
  "Whether sequences should be returned as list or vector.")

(defun items-in-sequence (element-function length)
  "Make a CL sequence of the type specified by *make-sequence-type*,
   computing each element with the function element-function."
  (let ((ans (make-sequence *make-sequence-type* length)))
    (dotimes (i length ans)
      (setf (elt ans i)
	    (funcall element-function i)))))

(defun pick-result (decl)
  (if (st-arrayp decl)		; eventually, check that it's a vector
      `((map *make-sequence-type*
	     (function
	      ,(cl-convert-function (st-eltype decl)))
	     (cffi::foreign-array-to-lisp
	      ,(st-symbol decl)
	      ',(st-eltype decl)
	      (list ,(st-dim decl)))))
      (case (st-type decl)
	(sf-result 
	 `((val ,(st-symbol decl))
	   (err ,(st-symbol decl))))
	(sf-result-e10
	 `((val ,(st-symbol decl) 'sf-result-e10)
	   (e10 ,(st-symbol decl))
	   (err ,(st-symbol decl) 'sf-result-e10)))
	(t `((,(cl-convert-function (st-type decl)) ,(st-symbol decl)))))))

(defun rearrange-sf-result-err (return-list)
  "Put the 'err values from the sf-results at the end of the return values."
  (flet ((sf-err (x)
	   (and (eq (first x) 'cffi:foreign-slot-value)
		(member (third x) '('sf-result 'sf-result-e10) :test #'equal)
		(equal (fourth x) ''err))))
    (append
     (remove-if #'sf-err return-list)
     (remove-if-not #'sf-err return-list))))

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

(defun check-null-pointers (check-null-pointers)
  (let ((cret (find :creturn check-null-pointers :key #'first)))
    (when cret
	`((check-null-pointer creturn ,@(rest cret))))))

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
	 (clargs (or arglist carg-symbs))
	 (cret-type (if (member c-return *special-c-return*)
			 :int
			 (if (listp c-return) (st-type c-return) c-return)))
	 (cret-name
	  (if (listp c-return) (st-symbol c-return) (make-symbol "CRETURN")))
	 (allocated		     ; Foreign objects to be allocated
	  (remove-if (lambda (s) (member s clargs)) carg-symbs))
	 (allocated-decl
	  (mapcar
	   (lambda (s) (find s cargs :key #'st-symbol))
	   allocated))
	 (clret (or (substitute cret-name :c-return return)
		    (mapcan #'pick-result allocated-decl)
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
	      ,@(check-null-pointers check-null-pointers)
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
			`(,@clret (success-failure ,cret-name)))
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
  (mapcan #'pick-result return-symb-type)))
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

(defun-gsl vector-minmax-index-new (v)
  "gsl_vector_minmax_index" (((pointer v) gsl-vector-c) (min :size) (max :size))
  :documentation
  "The indices of the minimum and maximum values in the vector @var{v}.
  When there are several equal minimum elements then the lowest index is
  returned."
  :c-return :void)

(defun-gsl set-all-new ((object gsl-vector) value)
  "gsl_vector_set_all"
  (((pointer object) :pointer) (value :double))
  :type :method
  :return ()
  :c-return :void)

(defun-gsl gsl-aref-new ((vector gsl-vector) &rest indices)
    "gsl_vector_get"
  (((pointer vector) :pointer) ((first indices) :size))
  :type :method
  :c-return :double
  :return (:c-return)
  :documentation "The ith element of the vector.")

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
