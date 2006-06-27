;********************************************************
; file:        blas1.lisp                                
; description: BLAS level 1, Vector operations
; date:        Wed Apr 26 2006 - 15:23                   
; author:      Liam Healy                                
; modified:    Mon Jun 26 2006 - 22:22
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Currently only includes double-float vector routines.
;;; Not ported: routines that use raw vectors gsl_blas_drotg, gsl_blas_drotmg, gsl_blas_drotm

;;; Doesn't work: swap, copy

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
;;;; Single
;;;;****************************************************************************

(defun-gsl sdot (result alpha vec1 vec2)
  "gsl_blas_sdsdot"
  ((alpha :float) ((pointer vec1) :pointer) ((pointer vec2) :pointer)
   ((gsl-array result) :pointer))
  :invalidate (result))

(defun-gsl dot ((vec1 gsl-vector-single) (vec2 gsl-vector-single))
  "gsl_blas_sdot"
  (((pointer vec1) :pointer) ((pointer vec2) :pointer) (result :float))
  :type :method)

(defun-gsl norm ((vec gsl-vector-single))
  "gsl_blas_snrm2"  (((pointer vec) :pointer))
  :type :method
  :c-return :float)

(defun-gsl asum ((vec gsl-vector-single))
  "gsl_blas_sasum" (((pointer vec) :pointer))
  :type :method
  :c-return :float)

(defun-gsl imax ((vec gsl-vector-single))
  "gsl_blas_isamax" (((pointer vec) :pointer))
  :type :method 
  :c-return :int)

;;;;****************************************************************************
;;;; Double
;;;;****************************************************************************

(defun-gsl dot ((vec1 gsl-vector-double) (vec2 gsl-vector-double))
  "gsl_blas_ddot"
  (((pointer vec1) :pointer) ((pointer vec2) :pointer) (result :double))
  :type :method)

(defun-gsl norm ((vec gsl-vector-double))
  "gsl_blas_dnrm2"  (((pointer vec) :pointer))
  :type :method
  :c-return :double)

(defun-gsl asum ((vec gsl-vector-double))
  "gsl_blas_dasum" (((pointer vec) :pointer))
  :type :method
  :c-return :double)

(defun-gsl imax ((vec gsl-vector-double))
  "gsl_blas_idamax" (((pointer vec) :pointer))
  :type :method 
  :c-return :int)

(defun-gsl swap ((vec1 gsl-vector-double) (vec2 gsl-vector-double))
  "gsl_blas_dswap" (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method 
  :invalidate (vec1 vec2))

(defun-gsl copy ((vec1 gsl-vector-double) (vec2 gsl-vector-double))
  "gsl_blas_dcopy" (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method
  :invalidate (vec2))

(defun-gsl axpy (alpha (vec1 gsl-vector-double) (vec2 gsl-vector-double))
  "gsl_blas_daxpy"
  ((alpha :double) ((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method 
  :invalidate (vec2))

(defun-gsl scal (alpha (vec gsl-vector-double))
  "gsl_blas_dscal" ((alpha :double) ((pointer vec) :pointer))
  :type :method 
  :invalidate (vec)
  :c-return :void)

(defun-gsl rot
    ((vec1 gsl-vector-double) (vec2 gsl-vector-double)
     (c double-float) (s double-float))
  "gsl_blas_drot"
  (((pointer vec1) :pointer) ((pointer vec2) :pointer) (c :double) (s :double))
  :type :method
  :invalidate (vec1 vec2))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test blas1
  ;; single
  (lisp-unit:assert-first-fp-equal
   "0.260000000000e+02"
   (with-data (a vector-single 3)
     (with-data (b vector-single 3)
       (setf (data a) #(1.0f0 2.0f0 3.0f0)
	     (data b) #(3.0f0 4.0f0 5.0f0))
       (dot a b))))
  (lisp-unit:assert-first-fp-equal
   "0.707106770000e+01"
   (with-data (b vector-single 3)
     (setf (data b) #(3.0 4.0 5.0))
     (norm b)))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000e+02"
   (with-data (b vector-single 3)
     (setf (data b) #(3.0 4.0 5.0))
     (asum b)))
  (lisp-unit:assert-eql
   1
   (with-data (b vector-single 3)
     (setf (data b) #(3.0f0 5.0f0 4.0f0))
     (imax b)))
  ;; double
  (lisp-unit:assert-first-fp-equal
   "0.260000000000d+02"
   (with-data (a vector-double 3)
     (with-data (b vector-double 3)
       (setf (data a) #(1.0d0 2.0d0 3.0d0)
	     (data b) #(3.0d0 4.0d0 5.0d0))
       (dot a b))))
  (lisp-unit:assert-first-fp-equal
   "0.707106781187d+01"
   (with-data (b vector-double 3)
     (setf (data b) #(3.0d0 4.0d0 5.0d0))
     (norm b)))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+02"
   (with-data (b vector-double 3)
     (setf (data b) #(3.0d0 4.0d0 5.0d0))
     (asum b)))
  (lisp-unit:assert-eql
   1
   (with-data (b vector-double 3)
     (setf (data b) #(3.0d0 5.0d0 4.0d0))
     (imax b))))
