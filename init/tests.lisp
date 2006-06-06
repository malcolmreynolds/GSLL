;********************************************************
; file:        tests.lisp                                
; description: Test functions.                           
; date:        Sat Apr 22 2006 - 16:52                   
; author:      Liam M. Healy                             
; modified:    Tue Jun  6 2006 - 09:45
;********************************************************
;;; $Id: $

;;; Interface with lisp-unit, add a definition for comparing floating
;;; point numbers and a form for generating floating point tests.

(in-package :lisp-unit)
(export 'assert-first-fp-equal)

(defparameter *test-fp-decimal-digits* 12
  "The number of decimal digits on which floating point
   number must agree.")

(defun fp-string (fp &optional (decimal-digits *test-fp-decimal-digits*))
  "Format the floating point number as a string for comparison."
  (format nil "~,v,2,0,,,ve"
	  decimal-digits (if (typep fp 'double-float) #\d #\e) fp))

(defmacro assert-first-fp-equal (expected form &rest extras)
  (lisp-unit::expand-assert
   :equal form `(fp-string (nth-value 0 ,form)) expected extras
   :test #'string-equal))

;;; (make-fp-test '(legendre-conicalP-half 3.5d0 10.0d0))
(defun gsl::make-fp-test (form)
  "Make a test."
  `(lisp-unit::assert-first-fp-equal
    ,(lisp-unit::fp-string (eval form))
    ,form))

