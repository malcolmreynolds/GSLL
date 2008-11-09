;; BLAS level 1, Vector operations
;; Liam Healy, Wed Apr 26 2006 - 15:23
;; Time-stamp: <2008-11-09 17:11:25EST blas1.lisp>
;; $Id$

(in-package :gsl)

(defmfun dot ((vec1 vector) (vec2 vector))
  ("gsl_blas_" :type "dot" :suffix)
  (((mpointer vec1) :pointer) ((mpointer vec2) :pointer)
   (result :element-c-type))
  :definition :generic
  :element-types :float-complex
  :documentation			; FDL
  "Dot, or inner, product between vectors.")

;;; gsl_blas_sdsdot doesn't make much sense, but here it is.
(defmfun sdot (result alpha vec1 vec2)
  "gsl_blas_sdsdot"
  ((alpha :float) ((mpointer vec1) :pointer) ((mpointer vec2) :pointer)
   (result :pointer))
  :outputs (result)
  :documentation			; FDL
  "Sum of a scalar and a dot product for single-floats.")

;;; Not porting gsl_blas_dsdot, stupid.

(defmfun cdot ((x vector) (y vector))
  ("gsl_blas_" :type "dotc")
  (((mpointer x) :pointer) ((mpointer y) :pointer)
   (result :element-c-type))
  :definition :generic
  :element-types :complex
  :inputs (x y)
  :documentation			; FDL
  "The complex conjugate scalar product x^H y for the vectors.")

(defmfun euclidean-norm ((vec vector))
  ("gsl_blas_" :component-float-type :type "nrm2")
  (((mpointer vec) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (vec)
  :c-return :component-float-type
  :documentation			; FDL
  "The Euclidean norm ||x||_2 = \sqrt {\sum x_i^2} of the vector x.")

(defmfun absolute-sum ((x vector))
  ("gsl_blas_" :component-float-type :type "asum")
  (((mpointer x) :pointer))
  :definition :generic
  :element-types :float-complex
  :c-return :component-float-type
  :inputs (x)
  :documentation			; FDL
  "The absolute sum \sum |x_i| of the elements of the vector x.")

(defmfun index-max ((vec vector))
  ("gsl_blas_i" :type "amax")
  (((mpointer vec) :pointer))
  :definition :generic
  :element-types :float-complex
  :c-return sizet
  :inputs (vec)
  :documentation			; FDL
  "The index of the largest element of the vector
   x. The largest element is determined by its absolute magnitude for
   real vectors and by the sum of the magnitudes of the real and
   imaginary parts |\Re(x_i)| + |\Im(x_i)| for complex vectors. If the
   largest value occurs several times then the index of the first
   occurrence is returned.")

(defmfun blas-swap ((vec1 vector) (vec2 vector))
  ("gsl_blas_" :type "swap")
  (((mpointer vec1) :pointer) ((mpointer vec2) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (vec1 vec2)
  :outputs (vec1 vec2)
  :documentation			; FDL
  "Exchange the elements of the vectors.")

(defmfun blas-copy ((x vector) (y vector))
  ("gsl_blas_" :type "copy")
  (((mpointer x) :pointer) ((mpointer y) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (x)
  :outputs (y)
  :return (y)
  :documentation			; FDL
  "Copy the elements of the vector x into the vector y.")

(defmfun axpy (alpha (x vector) (y vector))
  ;; This gets an error for complex types because you can't pass a
  ;; struct in CFFI yet.
  ("gsl_blas_" :type "axpy")
  ((alpha  :element-c-type) ((mpointer x) :pointer) ((mpointer y) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (x y)
  :outputs (y)
  :return (y)
  :documentation			; FDL
  "Compute the sum y = \alpha x + y for the vectors x and y.")

(defmfun scale ((alpha :element-type) (x vector))
  ;; Alpha is the same type as the elements of vector, so for complex
  ;; vectors it must be complex.
  ("gsl_blas_" :type "scal")
  ((alpha :element-c-type) ((mpointer x) :pointer))
  :definition :generic
  :element-types :float-complex
  :c-return :void
  :inputs (x)
  :outputs (x)
  :return (x)
  :documentation			; FDL
  "Rescale the vector x by the multiplicative factor alpha.")

(defmfun scale ((alpha :component-float-type) (x vector))
  ;; Alpha is a float and the vector is complex
  ("gsl_blas_" :type :component-float-type "scal")
  ((alpha :component-float-type) ((mpointer x) :pointer))
  :definition :methods
  :element-types :complex
  :c-return :void
  :inputs (x)
  :outputs (x)
  :return (x))

;;; The Givens rotations come in two forms, those that work on bare C
;;; arrays, and those that work on GSL vectors.  Ports of both are
;;; present.

(defmfun givens-rotation ((x vector) (y vector) (c vector) (s vector))
  ("gsl_blas_" :type "rotg")
  (((c-pointer x) :pointer) ((c-pointer y) :pointer)
   ((c-pointer c) :pointer) ((c-pointer s) :pointer))
  :definition :generic
  :element-types :float
  :inputs (x y c s)
  :outputs (x y)
  :return (x y)
  :documentation			; FDL
  "These functions compute a Givens rotation (c,s) to the vector (x,y),
          [  c  s ] [ x ] = [ r ]
          [ -s  c ] [ y ]   [ 0 ]
   The variables x and y are overwritten by the routine.")

(defmfun givens-rotation-m ((x vector) (y vector) (c vector) (s vector))
  ("gsl_blas_" :type "rot")
  (((mpointer x) :pointer) ((mpointer y) :pointer)
   ((mpointer c) :pointer) ((mpointer s) :pointer))
  :definition :generic
  :element-types :float
  :inputs (x y c s)
  :outputs (x y)
  :documentation			; FDL
  "These functions compute a Givens rotation (c,s) to the vector (x,y),
          [  c  s ] [ x ] = [ r ]
          [ -s  c ] [ y ]   [ 0 ]
   The variables x and y are overwritten by the routine.")

(defmfun modified-givens-rotation
    ((d1 vector) (d2 vector) (b1 vector) b2 (P vector))
  ("gsl_blas_" :type "rotmg")
  (((c-pointer d1) :pointer) ((c-pointer d1) :pointer)
   ((c-pointer b1) :pointer) (b2 :element-c-type)
   ((c-pointer P) :pointer))
  :definition :generic
  :element-types :float
  :inputs (d1 d2 b1 P)
  :outputs ()				; I have no idea
  :documentation
  "Not explained")

(defmfun modified-givens-rotation-m ((x vector) (y vector) (P vector))
  ("gsl_blas_" :type "rotm")
  (((mpointer x) :pointer) ((mpointer y) :pointer)
   ((mpointer P) :pointer))
  :definition :generic
  :element-types :float
  :inputs (x y P)
  :outputs (x y)			;?????
  :documentation	
  "Not explained")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(generate-all-array-tests dot :float-complex
 (letm ((v1 (array-default 8))
	(v2 (array-default 8)))
   (dot v1 v2)))

(generate-all-array-tests cdot :complex
 (letm ((v1 (array-default 8))
	(v2 (array-default 8)))
   (cdot v1 v2)))

(generate-all-array-tests euclidean-norm :float-complex
 (letm ((v1 (array-default 8)))
   (euclidean-norm v1)))

(generate-all-array-tests absolute-sum :float-complex
 (letm ((v1 (array-default 8)))
   (absolute-sum v1)))

(generate-all-array-tests index-max :float-complex
 (letm ((v1 (array-default 8)))
   (index-max v1)))

(generate-all-array-tests blas-swap :float-complex
 (letm ((v1 (array-default 3))
	(v2 (array-default 3)))
   (blas-swap v2 v1)
   (list (cl-array v1) (cl-array v2))))

(generate-all-array-tests blas-copy :float-complex
 (letm ((v1 (array-default 3))
	(v2 (array-default 3 t)))
   (blas-copy v1 v2)
   (cl-array v2)))

(generate-all-array-tests axpy :float-complex
 (letm ((v1 (array-default 8))
	(v2 (array-default 8))
	(scalar (scalar-default)))
   (cl-array (axpy scalar v1 v2))))

(generate-all-array-tests scale :float-complex
 (letm ((v1 (array-default 8))
	(scalar (scalar-default)))
   (cl-array (scale scalar v1))))

(generate-all-array-tests scale :complex
 (letm ((v1 (array-default 8))
	(scalar (scalar-default t)))
   (cl-array (scale scalar v1))))

(generate-all-array-tests givens :float
 (letm ((v1 (array-default 8))
	(v2 (array-default 8))
	(angles (array-default 8))
	(sines (array-default 8 t))
	(cosines (array-default 8 t)))
   (loop for i below 8 do
	(setf (maref sines i) (sin (maref angles i)))
	(setf (maref cosines i) (cos (maref angles i))))
   (givens-rotation v1 v2 cosines sines)
   (list (cl-array v1) (cl-array v2))))
