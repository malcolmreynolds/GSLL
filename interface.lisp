;********************************************************
; file:        interface.lisp                            
; description: Macros to interface GSL functions.
; date:        Mon Mar  6 2006 - 22:35                   
; author:      Liam M. Healy
; modified:    Sat Mar 25 2006 - 21:50
;********************************************************

(in-package :gsl)

(export '(sf-lookup))

;;;;****************************************************************************
;;;; GSL C structures
;;;;****************************************************************************

(cffi:defcstruct sf-result
  "Results from special functions with value and error estimate."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
  (val :double)
  (err :double))

(cffi:defcstruct sf-result-e10
  "Results from special functions with value, error estimate
and a scaling exponent e10, such that the value is val*10^e10."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
  (val :double)
  (err :double)
  (e10 :int))

(cffi:defcenum sf-mode
  "Numerical precision modes with which to calculate special functions."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC62
  :double-prec
  :single-prec
  :approx-prec)

;;;;****************************************************************************
;;;;  Numbers
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

;;;;****************************************************************************
;;;; Defining CL function names and mapping from C names
;;;;****************************************************************************

(defmacro defunx (name &rest args)
  "defun and export"
  `(progn
    (export '(,name))
    (defun ,name ,@args)))

(defparameter *sf-name-mapping* nil
  "An alist mapping the CL and GSL names of the special functions.
   This will only be populated when macros are expanded.")

(defun sf-lookup (string)
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

(defmacro defunx-map (cl-name gsl-name &rest args)
  "defun, export, and name-map."
  `(progn
    (setf *sf-name-mapping* (acons ',cl-name ',gsl-name *sf-name-mapping*))
    (defunx ,cl-name ,@args)))

;;;;****************************************************************************
;;;; Inputs and outputs
;;;;****************************************************************************

;;; An "rst" or return-symb-type as a list (symbol type) where
;;; type could be (element-type array-dim).  These are examples of lists
;;; of rsts: 
 ;; ((#:RET3500 SF-RESULT))
 ;; ((#:RET3501 (:DOUBLE (- NMAX NMIN)))) 
 ;; ((#:RET3502 (:DOUBLE (1+ KMAX))) (#:RET3503 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3504 (:DOUBLE (1+ KMAX))) (#:RET3505 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3506 :DOUBLE) (#:RET3507 :DOUBLE))

(defun rst-symbol (decl)
  (first decl))

(defun rst-type (decl)
  (second decl))

(defun rst-arrayp (decl)
  (listp (rst-type decl)))

(defun rst-eltype (decl)
  (first (rst-type decl)))

(defun rst-dim (decl)
  (second (rst-type decl)))

(defun pick-result (decl)
  (if (rst-arrayp decl)			; a vector of values
      `((loop for i from 0 below ,(rst-dim decl)
	      collect
	      ,(case (rst-eltype decl)
		     (gsl-complex
		      `(complex-to-cl ,(rst-symbol decl) i))
		     (double
		      `(double-to-cl ,(rst-symbol decl) i)))))
      (case (rst-type decl)
	(sf-result
	 `((cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result 'val)
	   (cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result 'err)))
	(sf-result-e10
	 `((cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result-e10 'val)
	   (cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result-e10 'e10)
	   (cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result-e10 'err)))
	(gsl-complex `((complex-to-cl ,(rst-symbol decl))))
	(:double `((double-to-cl ,(rst-symbol decl)))))))

;;;;****************************************************************************
;;;; Wrapping for arrays
;;;;****************************************************************************

(defun wrap-form (form wrappers)
  (if wrappers
      (wrap-form
       `(,@(first wrappers) ,form)
       (rest wrappers))
      form))

(defun wfa-wrapper (spec)
  "Returns two values: the foreign-funcall argument declaration
   and what to wrap around the form, if anything."
  (if (rst-arrayp spec)
      (let ((arr (gensym "ARR"))
	    (len (if (eq (rst-dim spec) '*)
		     (gensym "LEN")
		     (rst-dim spec))))
	(values
	 `(:pointer ,arr :size ,len)
	 `((cffi::with-foreign-array
	       (,arr ,(rst-symbol spec) ,(rst-eltype spec) (list ,len)))
	   (let ((,len (length ,(rst-symbol spec))))))))
      (values `(,(rst-type spec) ,(rst-symbol spec)) nil)))

(defun wfa-wrappers (specs)
  (if specs
      (multiple-value-bind (decl wrap)
	  (wfa-wrapper (first specs))
	(multiple-value-bind (decls wraps)
	    (wfa-wrappers (rest specs))
	  (values (append decl decls) (append wrap wraps))))
      (values nil nil)))

#+example
(multiple-value-bind (decl wrap)
    (wfa-wrappers
     '((foo :double) (coefficients (:double *)) (n :int) (boo (:double *))))
  (apply #'wrap-form `(the-body ,@decl) wrap))

#+example
(LET ((#:LEN4183 (LENGTH BOO)))
  (CFFI::WITH-FOREIGN-ARRAY (#:ARR4182 BOO :DOUBLE (LIST #:LEN4183))
    (LET ((#:LEN4181 (LENGTH COEFFICIENTS)))
      (CFFI::WITH-FOREIGN-ARRAY
	  (#:ARR4180 COEFFICIENTS :DOUBLE (LIST #:LEN4181))
	(THE-BODY :DOUBLE
		  FOO
		  :POINTER
		  #:ARR4180
		  :SIZE
		  #:LEN4181
		  :INT
		  N
		  :POINTER
		  #:ARR4182
		  :SIZE
		  #:LEN4183)))))

;;;;****************************************************************************
;;;; Macro defun-sf 
;;;;****************************************************************************

(defun wfo-declare (d)
  `(,(rst-symbol d)
    ,@(if (rst-arrayp d)
	  `(',(rst-eltype d) ,(rst-dim d))
	  `(',(rst-type d)))))

(defun check-gsl-status (status-code context)
  "Check the return status code from a GSL function and signal a warning
   if it is not :SUCCESS."
  (unless (eql :success (cffi:foreign-enum-keyword 'gsl-errorno status-code))
    (warn 'gsl-warning
	  :gsl-errno status-code :gsl-context context)))

;;; Warning isn't quite right for lambdas.
;;; New name?
(defmacro defun-sf
    (cl-name arguments gsl-name
     &key documentation return mode (c-return-value :error-code))
  "Define a CL function that provides an interface to a GSL function.
   If cl-name is :lambda, make a lambda.  Arguments:
     arguments:       a list of input arguments (symbol type) to the GSL function
     gsl-name:        the C function name, as a string
     documentation:   a string
     return:          a list of return types
     mode:            T or NIL, depending on whether gsl_mode is an argument
     c-return-value:  The C function returns an :error-code, :number-of-answers,
                      a value to :return from the CL function, or :void."
  (let ((clargs (mapcar #'rst-symbol arguments))
	(return-symb-type
	 (unless (eq c-return-value :return)
	   (mapcar (lambda (typ)
		     (list (gensym "RET")
			   typ))
		   return))))
    (multiple-value-bind (input-declarations wrap-arrays)
	(wfa-wrappers arguments)
      `(,@(if (eq cl-name :lambda)
	      '(lambda)
	      `(defunx-map ,cl-name ,gsl-name))
	  ,(if mode 
	       `(,@clargs &optional (mode :double-prec))
	       `(,@clargs))
	  ,@(when documentation (list documentation))
	  ,(wrap-form			; with-foreign-array
	    (wrap-form 			; with-foreign-object
	     `(let ((creturn
		     (cffi:foreign-funcall
		      ,gsl-name
		      ,@input-declarations
		      ,@(when mode '(sf-mode mode))
		      ,@(mapcan (lambda (r) `(:pointer ,(rst-symbol r)))
				return-symb-type)
		      ,(if (eq c-return-value :return)
			   (first return)
			   :int))))
		,@(case c-return-value
			(:void '((declare (ignore creturn))))
			(:error-code
			 `((check-gsl-status creturn `(,',cl-name ,,@clargs)))))
		(values
		 ,@(case c-return-value
			 (:number-of-answers
			  (mapcan
			   (lambda (decl seq)
			     `((when (> creturn ,seq) ,@(pick-result decl))))
			   return-symb-type
			   (loop for i below (length return) collect i)))
			 (:return '(creturn))
			 (:error-code
			  (mapcan #'pick-result return-symb-type)))))
	     (when return-symb-type
	       `((cffi:with-foreign-objects
		     ,(mapcar #'wfo-declare return-symb-type)))))
	    wrap-arrays)))))

;;; arguments: list like ((x :double) (y :double))
;;; mode: t or nil
