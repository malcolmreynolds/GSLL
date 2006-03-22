;********************************************************
; file:        interface.lisp                            
; description: Macros to interface GSL functions.
; date:        Mon Mar  6 2006 - 22:35                   
; author:      Liam M. Healy
; modified:    Wed Mar 22 2006 - 12:20
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

(cffi:defcstruct gsl-complex
  "A complex number in GSL."
  (dat :double :count 2))

(cffi:defcenum sf-mode
  "Numerical precision modes with which to calculate special functions."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC62
  :double-prec
  :single-prec
  :approx-prec)

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
;;;; Macro defun-sf 
;;;;****************************************************************************

(defun pick-result (decl)
  (if (listp (second decl))
      `((loop for i from 0 below ,(second (second decl))
	      collect (cffi:mem-aref ,(first decl) ,(first (second decl)) i)))
      (case (second decl)
	(sf-result
	 `((cffi:foreign-slot-value ,(first decl) 'sf-result 'val)
	   (cffi:foreign-slot-value ,(first decl) 'sf-result 'err)))
	(sf-result-e10
	 `((cffi:foreign-slot-value ,(first decl) 'sf-result-e10 'val)
	   (cffi:foreign-slot-value ,(first decl) 'sf-result-e10 'e10)
	   (cffi:foreign-slot-value ,(first decl) 'sf-result-e10 'err)))
	(gsl-complex
	 `((let ((carr
		  (cffi:foreign-slot-value ,(first decl) 'gsl-complex 'dat)))
	     (complex
	      (cffi:mem-aref carr :double 0)
	      (cffi:mem-aref carr :double 1)))))
	(:double `((cffi:mem-ref ,(first decl) :double))))))

(defun wfo-declare (d)
  `(,(first d)
     ,@(if (symbolp (second d))
	   `(',(second d))
	   `(',(first (second d)) ,(second (second d))))))

(defun check-gsl-status (status-code context)
  "Check the return status code from a GSL function and signal a warning
   if it is not :SUCCESS."
  (unless (eql :success (cffi:foreign-enum-keyword 'gsl-errorno status-code))
    (warn 'gsl-warning
	  :gsl-errno status :gsl-context context)))

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
     c-return-value:  The C function returns an :error-code or :number-of-answers."
  (let ((args (mapcar #'first arguments))
	(return-symb-type
	 (mapcar (lambda (typ)
		   (list (gensym "RET")
			 typ))
		 return)))
    ;; return-symb-type like
    ;; ((#:RET3500 SF-RESULT))
    ;; ((#:RET3501 (:DOUBLE (- NMAX NMIN)))) 
    ;; ((#:RET3502 (:DOUBLE (1+ KMAX))) (#:RET3503 (:DOUBLE (1+ KMAX)))
    ;;  (#:RET3504 (:DOUBLE (1+ KMAX))) (#:RET3505 (:DOUBLE (1+ KMAX)))
    ;;  (#:RET3506 :DOUBLE) (#:RET3507 :DOUBLE))
    `(,@(if (eq cl-name :lambda)
	    '(lambda)
	    `(defunx-map ,cl-name ,gsl-name))
      ,(if mode 
	   `(,@args &optional (mode :double-prec))
	   `(,@args))
      ,@(when documentation (list documentation))
      (cffi:with-foreign-objects
	  ,(mapcar #'wfo-declare return-symb-type)
	(let ((status
	       (cffi:foreign-funcall
		,gsl-name
		,@(mapcan (lambda (ar) (list (second ar) (first ar)))
			  arguments)
		,@(when mode '(sf-mode mode))
		,@(mapcan (lambda (r) `(:pointer ,(first r))) return-symb-type)
		:int)))
	  ,@(if (eq c-return-value :error-code)
		`((check-gsl-status status `(,',cl-name ,,@args))))
	  (values
	   ,@(if (eq c-return-value :number-of-answers)
		 (mapcan
		  (lambda (decl seq)
		    `((when (> status ,seq) ,@(pick-result decl))))
		  return-symb-type
		  (loop for i below (length return) collect i))
		 (mapcan #'pick-result return-symb-type))))))))

;;; arguments: list like ((x :double) (y :double))
;;; mode: t or nil
