;********************************************************
; file:        tests.lisp                                
; description: Test functions.                           
; date:        Sat Apr 22 2006 - 16:52                   
; author:      Liam M. Healy                             
; modified:    Tue Jul 18 2006 - 22:36
;********************************************************
;;; $Id$

;;; Interface with lisp-unit, add a definition for comparing floating
;;; point numbers and a form for generating floating point tests.

(in-package :lisp-unit)

(defparameter *zero-threshold* 1.0d-12
  "Threshold below which a number is to be considered zero.")

(defparameter *acceptable-fraction-error* 1.0d-12
  "Fractional error which is considered acceptable when
  comparing floating point numbers.")

(defun fp-equal (fp1 fp2)
  "The floats fp1 and fp2 are to be considered equal."
  ;; For now, fp1 and fp2 must be actual floats and not nans/infs.
  (or (and (<= (abs fp1) *zero-threshold*)
	   (<= (abs fp2) *zero-threshold*))
      (<= (abs (/ (- fp1 fp2) fp1)) *acceptable-fraction-error*)))

(defun numerical-equal (result1 result2)
  (and
   (or
     (typep result1 'vector) (typep result2 'vector)
     (typep result1 'complex) (typep result2 'complex)
     (typep result1 'array) (typep result2 'array)
     (eql (type-of result1) (type-of result2)))
   (typecase result1
     (integer (= result1 result2))
     (float (fp-equal result1 result2))
     (complex (and (fp-equal (realpart result1) (realpart result2))
		   (fp-equal (imagpart result1) (imagpart result2))))
     (sequence
      (and (eql (length result1) (length result2))
	   (every #'numerical-equal result1 result2)))
     (array
      (and (= (array-rank result1) (array-rank result2) 2)
	   (loop for i below (array-dimension result1 0)
		 always
		 (loop for j below (array-dimension result1 1)
		       always (numerical-equal (aref result1 i j)
					       (aref result2 i j)))))))))

(defun numerical-serialize (form)
  (if (typep form 'list)
      (cons 'list (mapcar #'numerical-serialize form))
      form))

(defmacro assert-numerical-equal (expected form &rest extras)
  (lisp-unit::expand-assert
    :equal form form expected extras
    :test #'numerical-equal))


;;; (make-test '(legendre-conicalP-half 3.5d0 10.0d0))
(defun gsl::make-test (form)
  "Make a test for lisp-unit."
  (let ((vals (multiple-value-list (ignore-errors (eval form)))))
    (if (typep (second vals) 'condition)
	`(lisp-unit::assert-error
	  ',(type-of (second vals))
	  ,form)
	`(lisp-unit::assert-numerical-equal
	  ,(numerical-serialize vals)
	  (multiple-value-list ,form)))))

(defmacro gsl::make-tests (name &rest forms)
  (append
   `(lisp-unit:define-test ,name)
   (mapcar #'gsl::make-test forms)))
