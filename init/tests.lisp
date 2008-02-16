;********************************************************
; file:        tests.lisp                                
; description: Test functions.                           
; date:        Sat Apr 22 2006 - 16:52                   
; author:      Liam M. Healy                             
; modified:    Tue Jul 18 2006 - 22:36
;********************************************************
;;; $Id: $

;;; Interface with lisp-unit, add a definition for comparing floating
;;; point numbers and a form for generating floating point tests.

(in-package :lisp-unit)
(export '(assert-first-fp-equal fp-values fp-sequence))

(defparameter *test-fp-decimal-digits* 12
  "The number of decimal digits on which floating point
   number must agree.")

(defparameter *zero-threshold* 1.0d-15
  "Threshold below which a number is tpo be considered zero.")

(defun fp-string (fp &optional (decimal-digits *test-fp-decimal-digits*))
  "Format the floating point number as a string for comparison."
  (if (typep fp 'complex)
      (list (fp-string (realpart fp)) (fp-string (imagpart fp)))
      (format nil "~,v,2,0,,,ve"
	      decimal-digits (if (typep fp 'double-float) #\d #\e)
	      (if (< (abs fp)  *zero-threshold*)
		  0.0d0
		  fp))))

(defmacro assert-first-fp-equal (expected form &rest extras)
  (lisp-unit::expand-assert
   :equal form `(fp-string (nth-value 0 ,form)) expected extras
   :test #'string-equal))

(defmacro fp-values (results)
  `(mapcar #'fp-string (multiple-value-list ,results)))

(defun fp-sequence (results)
  (map 'list #'fp-string results))


;;(gsl:double-float-unequal x y double-float-epsilon)

;;; (make-fp-test '(legendre-conicalP-half 3.5d0 10.0d0))
(defun gsl::make-fp-test (form)
  "Make a test."
  `(lisp-unit::assert-first-fp-equal
    ,(lisp-unit::fp-string (eval form))
    ,form))

;;;;;;;;;;;;;;;;;
;;; Thinking about how to do floating point comparisons

(defun gsl::equal-float (x rat)
  "Is the float very close to the rational?"
  (<= (abs (- rat (rationalize x)))
      (* 4 (if (typep x 'double-float)
	       double-float-epsilon
	       single-float-epsilon))))

;;; Tests:
;;; assert-float-equal: First value only
;;; turn multiple values into a list
;;; compare sequences including structure.
