;; Generate matrices used in tests of linear algebra functions
;; Liam Healy 2009-09-19 18:28:31EDT matrix-generation.lisp
;; Time-stamp: <2009-09-26 16:42:11EDT matrix-generation.lisp>

(in-package :gsl)

;;; These are general to all the linear solver techniques, so more
;;; tests need to be made.  The symbols are not exported because it
;;; assumed they will only be used internally by the test functions.

;;; See linalg/test.c.

;;;;****************************************************************************
;;;; General array creation from indices
;;;;****************************************************************************

;;; These should be exported.  Come to think of it, didn't Glen
;;; have something more general than this?
(defun set-matrix (matrix function)
  (dotimes (i (dim0 matrix) matrix)
    (dotimes (j (dim1 matrix))
      (setf (maref matrix i j)
	    (coerce (funcall function i j) (element-type matrix))))))

(defun create-matrix
    (function dim0 &optional (dim1 dim0) (element-type 'double-float))
  "Make a matrix of the specified dimensions, with contents
   based on a function of the element indices i, j."
  (set-matrix
   (make-marray (cl-single element-type) :dimensions (list dim0 dim1))
   function))

(defun create-vector
    (function dim &optional (element-type 'double-float))
  "Make a vector of the specified dimension, with contents
   based on a function of the element index."
  (let ((vector
	 (make-marray (cl-single element-type) :dimensions dim)))
    (dotimes (i dim vector)
      (setf (maref vector i)
	    (coerce (funcall function i) element-type)))))

(defun create-diagonal-matrix (vector)
  "Place the vector along the diagonal of square matrix."
  (create-matrix
   (lambda (i j) (if (= i j) (maref vector i) 0))
   (dim0 vector)))

(defun constant-matrix
    (constant dim0 &optional (dim1 dim0) (element-type 'double-float))
  (let ((cst (coerce constant element-type)))
    (create-matrix (lambda (i j) (declare (ignore i j)) cst)
		   dim0 dim1 element-type)))

;;;;****************************************************************************
;;;; Specific arrays used in linear algebra tests
;;;;****************************************************************************

(defun create-general-matrix (dim0 dim1)
  (create-matrix (lambda (i j) (/ (+ 1 i j))) dim0 dim1))

(defun create-singular-matrix (dim0 dim1)
  (create-matrix (lambda (i j) (if (zerop i) 0 (/ (+ 1 i j)))) dim0 dim1))

(defun create-hilbert-matrix (dim)
  "Make Hilbert matrix used to test linear algebra functions."
  (create-general-matrix dim dim))

(defun create-vandermonde-matrix (dim)
  "Make Van der Monde matrix used to test linear algebra functions."
  (create-matrix (lambda (i j) (expt (1+ i) (- dim j 1))) dim))

(defun create-moler-matrix (dim)
  (create-matrix (lambda (i j) (- (min (1+ i) (1+ j)) 2)) dim))

(defun create-row-matrix (dim0 dim1)
  ;; This would be better named a column matrix, but they call it a row.
  (create-matrix (lambda (i j) (if (zerop j) (/ (1+ i)) 0)) dim0 dim1))

(defun create-complex-matrix (dim)
  (create-matrix
   (lambda (i j)
     (complex (/ (+ 1 i j)) (+ 1/2 (expt i 2) (expt j 2))))
   dim dim '(complex double-float)))

(defun create-rhs-vector (dim &optional (element-type 'double-float))
  (if (subtypep element-type 'complex)
      (create-vector
       (lambda (i) (complex (1+ (* 2 i)) (+ 2 (* 2 i)))) 7 element-type)
      (create-vector '1+ dim element-type)))

(defparameter *hilb2* (create-hilbert-matrix 2))
(defparameter *hilb3* (create-hilbert-matrix 3))
(defparameter *hilb4* (create-hilbert-matrix 4))
(defparameter *hilb12* (create-hilbert-matrix 12))

(defparameter *hilb2-soln*
  (make-marray 'double-float :initial-contents '(-8.0d0 18.0d0)))
(defparameter *hilb3-soln*
  (make-marray 'double-float :initial-contents '(27.0d0 -192.0d0 210.0d0)))
(defparameter *hilb4-soln*
  (make-marray 'double-float
	       :initial-contents '(-64.0d0 900.0d0 -2520.0d0 1820.0d0)))
(defparameter *hilb12-soln*
  (make-marray 'double-float :initial-contents
	       '(-1728.0d0 245388.0d0 -8528520.0d0
		 127026900.0d0 -1009008000.0d0 4768571808.0d0
		 -14202796608.0d0 27336497760.0d0 -33921201600.0d0
		 26189163000.0d0 -11437874448.0d0 2157916488.0d0)))

(defparameter *vander2* (create-vandermonde-matrix 2))
(defparameter *vander3* (create-vandermonde-matrix 3))
(defparameter *vander4* (create-vandermonde-matrix 4))
(defparameter *vander12* (create-vandermonde-matrix 12))

(defparameter *vander2-soln*
  (make-marray 'double-float :initial-contents '(1.0d0 0.0d0)))
(defparameter *vander3-soln*
  (make-marray 'double-float :initial-contents
	       '(0.0d0 1.0d0 0.0d0)))
(defparameter *vander4-soln*
  (make-marray 'double-float :initial-contents
	       '(0.0d0 0.0d0 1.0d0 0.0d0)))
(defparameter *vander12-soln*
  (make-marray 'double-float :initial-contents
	       '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
		 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0)))

(defparameter *m35* (create-general-matrix 3 5))
(defparameter *m53* (create-general-matrix 5 3))
(defparameter *s35* (create-singular-matrix 3 5))
(defparameter *s53* (create-singular-matrix 5 3))
