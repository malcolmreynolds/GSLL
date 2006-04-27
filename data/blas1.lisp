;********************************************************
; file:        blas1.lisp                                
; description: BLAS level 1, Vector operations
; date:        Wed Apr 26 2006 - 15:23                   
; author:      Liam Healy                                
; modified:    Wed Apr 26 2006 - 21:54
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Currently only includes double-float vector routines.
;;; Not ported: routines that use raw vectors gsl_blas_drotg, gsl_blas_drotmg, gsl_blas_drotm

;;;;****************************************************************************
;;;; Generic
;;;;****************************************************************************

(export '(dot norm asum imax swap copy axpy scal rot))

(defgeneric dot (vector1 vector2)
  (:documentation "Dot, or inner, product between vectors."))

(defgeneric norm (x)
  (:documentation "The Euclidean norm ||x||_2 = \sqrt {\sum x_i^2} of the vector x."))

(defgeneric asum (x)
  (:documentation "The absolute sum \sum |x_i| of the elements of the vector x."))

(defgeneric imax (x)
  (:documentation "The index of the largest element of the vector
   x. The largest element is determined by its absolute magnitude for
   real vectors and by the sum of the magnitudes of the real and
   imaginary parts |\Re(x_i)| + |\Im(x_i)| for complex vectors. If the
   largest value occurs several times then the index of the first
   occurrence is returned."))

(defgeneric swap (x y)
  (:documentation "Exchange the elements of the vectors x and y"))

(defgeneric copy (x y)
  (:documentation "Copy the elements of the vector x into the vector y."))

(defgeneric axpy (alpha x y)
  (:documentation "The sum y = \alpha x + y for the vectors x and y."))

(defgeneric scal (alpha x)
  (:documentation "Rescale the vector x by the multiplicative factor alpha."))

(defgeneric rot (x y c s)
  (:documentation
   "Apply a Givens rotation (x', y') = (c x + s y, -s x + c y) to the vectors x, y."))

;;;;****************************************************************************
;;;; Double
;;;;****************************************************************************

(defun-gsl dot (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  "gsl_blas_ddot"
  :method
  ((vec1 gsl-vector) (vec2 gsl-vector))
  :return (:double))

(defun-gsl norm (((pointer vec) :pointer))
  "gsl_blas_dnrm2"
  :method
  ((vec gsl-vector))
  :c-return-value :return
  :return (:double))

(defun-gsl asum (((pointer vec) :pointer))
  "gsl_blas_dasum"
  :method
  ((vec gsl-vector))
  :c-return-value :return
  :return (:double))

(defun-gsl imax (((pointer vec) :pointer))
  "gsl_blas_idamax"
  :method ((vec gsl-vector))
  :c-return-value :return
  :return (:int))

(defun-gsl swap (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  "gsl_blas_dswap"
  :method ((vec1 gsl-vector) (vec2 gsl-vector))
  :after ((cl-invalidate vec2)))

(defun-gsl copy (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  "gsl_blas_dcopy"
  :method ((vec1 gsl-vector) (vec2 gsl-vector))
  :after ((cl-invalidate vec2)))

(defun-gsl axpy
    ((alpha :double) ((pointer vec1) :pointer) ((pointer vec2) :pointer))
  "gsl_blas_daxpy"
  :method (alpha (vec1 gsl-vector) (vec2 gsl-vector))
  :after ((cl-invalidate vec2)))

(defun-gsl scal ((alpha :double) ((pointer vec) :pointer))
  "gsl_blas_dscal"
  :method (alpha (vec gsl-vector))
  :after ((cl-invalidate vec))
  :c-return-value :void)

(defun-gsl rot
    (((pointer vec1) :pointer) ((pointer vec2) :pointer) (c :double) (s :double))
  "gsl_blas_drot"
  :method ((vec1 gsl-vector) (vec2 gsl-vector) (c double-float) (s double-float))
  :after ((cl-invalidate vec1 vec2)))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

