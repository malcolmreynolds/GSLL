;********************************************************
; file:        interface.lisp                            
; description: Macros to interface GSL functions.
; date:        Mon Mar  6 2006 - 22:35                   
; author:      Liam M. Healy
; modified:    Wed May 24 2006 - 23:35
;********************************************************

(in-package :gsl)

(export '(gsl-lookup *make-sequence-type*))

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

(defun size-to-cl (size &optional (index 0))
  (cffi:mem-aref size :size index))

(defun cl-convert-function (type)
  (case type
    (:double 'double-to-cl)
    (:size 'size-to-cl)
    (gsl-complex 'complex-to-cl)))

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

(defmacro defunx-map (cl-name gsl-name &rest args)
  "defun, export, and name-map."
  `(progn
    (map-name ',cl-name ',gsl-name)
    (defunx ,cl-name ,@args)))

(defmacro defmethod-map (cl-name gsl-name &rest args)
  "defmethod and name-map."
  `(progn
    (map-name ',cl-name ',gsl-name)
    (defmethod ,cl-name ,@args)))

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

(defun rst-array-pointer-last-p (decl)
  (listp (rst-type decl)))

(defun rst-eltype (decl)
  (first (rst-type decl)))

(defun rst-dim (decl)
  (second (rst-type decl)))

(defun rst-pointer-last-p (decl)
  (third (rst-type decl)))

(defun return-symbol-type (return)
  "Make a full declaration from the list of return values."
  (mapcar (lambda (typ)
	    (list (gensym "RET")
		  typ))
	  return))

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
  (if (rst-arrayp decl)		; eventually, check that it's a vector
      `((map *make-sequence-type*
	 (function
	  ,(cl-convert-function (rst-eltype decl)))
	 (cffi::foreign-array-to-lisp
	  ,(rst-symbol decl)
	  ',(rst-eltype decl)
	  (list ,(rst-dim decl)))))
      (case (rst-type decl)
	(sf-result
	 `((cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result 'val)
	   (cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result 'err)))
	(sf-result-e10
	 `((cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result-e10 'val)
	   (cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result-e10 'e10)
	   (cffi:foreign-slot-value ,(rst-symbol decl) 'sf-result-e10 'err)))
	(t `((,(cl-convert-function (rst-type decl)) ,(rst-symbol decl)))))))

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
;;;; Wrapping of input arguments
;;;;****************************************************************************

(defparameter *wrap-types* nil
  "An alist of type and wrap function for values to and from C.")

(defmacro add-wrap-type (type function)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *wrap-types* (acons ',type ,function *wrap-types*))))

(defun wrap-arg (spec)
  "Wrap the symbol with a form that has been specified for
   the type of argument."
  (let ((wrapform (rest (assoc (rst-type spec) *wrap-types*))))
    (if wrapform
	(funcall wrapform (rst-symbol spec))
	(rst-symbol spec))))

(defun wrap-form (form wrappers)
  (if wrappers
      (wrap-form
       (if (first wrappers) `(,@(first wrappers) ,form) form)
       (rest wrappers))
      form))

;;; All arrays and vectors are passed with a gsl-data object that is made
;;; outside the function, and pieces are spliced in the function call.
;;; If a raw array is needed, use the :function argument to defun-gsl,
;;; then use the appropriate function(s) e.g. gsl-array, dim0
;;; on that array to map to the GSL arguments.
(defun splice-arguments (arguments &optional mode)
  "Convert the argument declarations to a list of declarations appropriate
   for foreign-funcall.  If mode is T, a mode argument will be added to the end,
   if it is an integer, it will be put at that position."
  (flet ((splicearg (spec)
	   `(,(rst-type spec)
	     ,(wrap-arg spec))))
    (mapcan #'splicearg
	    (if mode
		(let ((mode (if (integerp mode) mode (length arguments))))
		  (append (subseq arguments 0 mode)
			  '((mode sf-mode))
			  (subseq arguments mode)))
		arguments))))

;;;;****************************************************************************
;;;; Checking results
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

(defun wfo-declare (d)
  `(,(rst-symbol d)
    ,@(if (rst-arrayp d)
	  `(',(rst-eltype d) ,(rst-dim d))
	  `(',(rst-type d)))))

;;; Warning isn't quite right for lambdas.
(defmacro defun-gsl
    (cl-name arguments gsl-name
     &key documentation return mode (c-return-value :error-code)
     return-input check-null-pointers function method invalidate after
     (multiple-returns (lambda (&rest args) `(values ,@args))))
  "Define a CL function that provides an interface to a GSL function.
   If cl-name is :lambda, make a lambda.  Arguments:
     arguments:       a list of input arguments (symbol type) to the GSL function
     gsl-name:        the C function name, as a string
     documentation:   a string
     return:          a list of return types
     return-input:    input variables to return
     mode:            T, NIL or position, depending on whether
                      gsl_mode is an argument
     c-return-value:  The C function returns an :error-code, :number-of-answers,
                      a value to :return from the CL function, a
                      :success-failure code to be returned from CL as T or NIL,
                      or :void.
     check-null-pointers:
                      a list of return variables that should be checked,
                      if a null pointer, signal an error.
     method           Make output a defmethod with the value as the arglist;
                      'arguments should then include explicit mapping of all arguments
                      to GSL form.
     function         Arguments for CL function (like :method, but make a function).
     invalidate       CL copies of data to invalidate before return.
     after            Functions to call after the GSL function has been called;
                      result is discarded.
     multiple-returns      Function that wraps a form around return arguments."
  (let ((clargs (or function method (mapcar #'rst-symbol arguments)))
	(return-symb-type 
	 (unless (or (eq c-return-value :return) return-input)
	   (return-symbol-type return))))
    (multiple-value-bind (cargs dimbind)
	(splice-arguments arguments mode)
      `(,@(if (eq cl-name :lambda)
	      '(lambda)
	      `(,(if method 'defmethod-map 'defunx-map) ,cl-name ,gsl-name))
	,(if mode 
	     `(,@clargs &optional (mode :double-prec))
	     `(,@clargs))
	,@(when documentation (list documentation))
	,(wrap-form 			; with-foreign-object
	  `(let ((creturn
		  (cffi:foreign-funcall
		   ,gsl-name
		   ,@cargs
		   ,@(mapcan (lambda (r) `(:pointer ,(rst-symbol r)))
			     return-symb-type)
		   ,(case c-return-value
			  (:return (first return))
			  (:void :void)
			  (t :int)))))
	    ,@(case c-return-value
		    (:void '((declare (ignore creturn))))
		    (:error-code
		     (if (or function method)
			 `((check-gsl-status creturn `(,',cl-name))) ; need args
			 `((check-gsl-status creturn `(,',cl-name ,,@clargs))))))
	    ,@(check-null-pointers check-null-pointers)
	    ,@(when invalidate `((cl-invalidate ,@invalidate)))
	    ,@after
	    ,(apply (eval multiple-returns)
		    (append return-input
			    (case c-return-value
			      (:number-of-answers
 			       (mapcan
				(lambda (decl seq)
				  `((when (> creturn ,seq) ,@(pick-result decl))))
				return-symb-type
				(loop for i below (length return) collect i)))
			      (:success-failure
			       '((success-failure creturn)))
			      (:return (list (wrap-arg `(creturn ,@return))))
			      (t
			       (rearrange-sf-result-err
				(mapcan #'pick-result return-symb-type)))))))
	  (list
	   (when return-symb-type
	     `(cffi:with-foreign-objects
	       ,(mapcar #'wfo-declare return-symb-type)))
	   (when dimbind `(let ,dimbind))))))))
