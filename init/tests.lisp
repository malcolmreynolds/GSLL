;********************************************************
; file:        tests.lisp                                
; description: Test functions.                           
; date:        Sat Apr 22 2006 - 16:52                   
; author:      Liam M. Healy                             
; modified:    Sat Apr 22 2006 - 17:31
;********************************************************
;;; $Id: $

(in-package :lisp-unit)

(defparameter *test-fp-decimal-digits* 12
  "The number of decimal digits on which floating point
   number must agree.")

(defun fp-string (fp &optional (decimal-digits *test-fp-decimal-digits*))
  "Format the floating point number as a string for comparison."
  (format nil "~,v,2,0e" decimal-digits fp))

(defun fp-result
    (fp string &optional (decimal-digits *test-fp-decimal-digits*))
  (string-equal (fp-string fp decimal-digits) string))

(export 'assert-first-fp-equal)
(defmacro assert-first-fp-equal (expected form &rest extras)
  (lisp-unit::expand-assert
   :equal form `(fp-string (nth-value 0 ,form)) expected extras
   :test #'string-equal))
