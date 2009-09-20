;; Generate matrices used in tests of linear algebra functions
;; Liam Healy 2009-09-19 18:28:31EDT matrix-generation.lisp
;; Time-stamp: <2009-09-19 21:51:22EDT matrix-generation.lisp>

(in-package :gsl)

;;; These are general to all the linear solver techniques, so more
;;; tests need to be made.  The symbols are not exported because it
;;; assumed they will only be used internally by the test functions.

;;; See linalg/test.c.

;;; Maybe this should be exported.  Come to think of it, didn't Glen
;;; have something more general than this?
(defun create-matrix
    (function dim0 &optional (dim1 dim0) (eltype 'double-float))
  "Make a matrix of the specified dimensions, with contents
   based on a function of the element indices i, j."
  (let ((matrix
	 (make-marray (cl-single eltype) :dimensions (list dim0 dim1))))
    (dotimes (i dim0 matrix)
      (dotimes (j dim1)
	(setf (maref matrix i j)
	      (coerce (funcall function i j) eltype))))))

(defun create-general-matrix (dim0 dim1)
  (create-matrix (lambda (i j) (/ (+ 1 i j))) dim0 dim1))

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

;;; This should be exported too.
(defun create-diagonal-matrix (vector)
  "Place the vector along the diagonal of square matrix."
  (create-matrix
   (lambda (i j) (if (= i j) (maref vector i) 0))
   (dim0 vector)))

(defun create-complex-matrix (dim)
  (create-matrix
   (lambda (i j)
     (complex (/ (+ 1 i j)) (+ 1/2 (expt i 2) (expt j 2))))
   dim dim '(complex double-float)))

;;; Create a vector too
(defun create-vector (dim)
  (let ((vec (make-marray 'double-float :dimensions dim)))
    (dotimes (i dim vec)
      (setf (maref vec i) (coerce (1+ i) 'double-float)))))
