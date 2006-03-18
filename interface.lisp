;********************************************************
; file:        interface.lisp                            
; description: Macros to interface GSL functions.
; date:        Mon Mar  6 2006 - 22:35                   
; author:      Liam M. Healy
; modified:    Fri Mar 17 2006 - 21:34
;********************************************************

(in-package :gsl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '()))

;;; Need CFFI
;;; http://common-lisp.net/project/cffi/

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

(defun pick-result (decl)
  (if (listp (second decl))
      `((loop for i from 0 to ,(second (second decl))
	  collect (cffi:mem-aref ,(first decl) ,(first (second decl)) i)))
      (case (second decl)
	(sf-result
	 `((cffi:foreign-slot-value ,(first decl) 'sf-result 'val)
	   (cffi:foreign-slot-value ,(first decl) 'sf-result 'err)))
	(:double `((cffi:mem-ref ,(first decl) :double))))))

(defun first-or-self (x)
  (if (listp x)
      (first x)
      x))

;;; Warning isn't quite right for lambdas.
;;; New name?
(defmacro defun-sf
    (cl-name arguments gsl-name &key documentation return mode)
  "Define a mathematical special function from GSL using the _e form
   GSL function definition.  If cl-name is :lambda, make a lambda."
  (let ((args (mapcar #'first arguments))
	(return-symb-type
	 (mapcar (lambda (typ)
		   (list (gensym "RET")
			 typ))
		 return)))
    `(,@(if (eq cl-name :lambda)
	    '(lambda)
	    `(defun ,cl-name))
	,(if mode 
	     `(,@args &optional (mode :double-prec))
	     `(,@args))
	,@(when documentation (list documentation))
	(cffi:with-foreign-objects
	    (,@(mapcar (lambda (d) `(,(first d) ',(first-or-self (second d))))
		       return-symb-type))
	  (let ((status
		 (cffi:foreign-funcall
		  ,gsl-name
		  ,@(mapcan (lambda (ar) (list (second ar) (first ar)))
			    arguments)
		  ,@(when mode '(sf-mode mode))
		  ,@(mapcan (lambda (r) `(:pointer ,(first r))) return-symb-type)
		  :int)))
	    (unless (eql :success (cffi:foreign-enum-keyword 'gsl-errorno status))
	      (warn 'gsl-warning
		    :gsl-errno status :gsl-context `(,',cl-name ,,@args)))
	    (values
	     ,@(mapcan #'pick-result return-symb-type)))))))


;;; arguments: list like ((x :double) (y :double))
;;; mode: t or nil


