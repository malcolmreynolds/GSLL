;; BLAS level 1, Vector operations
;; Liam Healy, Wed Apr 26 2006 - 15:23
;; Time-stamp: <2008-08-23 14:34:44EDT blas1.lisp>
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

(defmfun euclidean-norm ((vec1 vector) (vec2 vector))
  ("gsl_blas_" :component-float-type :type "nrm2")
  (((mpointer vec1) :pointer) ((mpointer vec2) :pointer)
   (result :component-float-type))
  :definition :generic
  :element-types :float-complex
  :inputs (vec1 vec2)
  :documentation			; FDL
  "The Euclidean norm ||x||_2 = \sqrt {\sum x_i^2} of the vector x.")

(defmfun absolute-sum ((x vector))
  ("gsl_blas_" :component-float-type :type "asum")
  (((mpointer x) :pointer)
   (result :component-float-type))
  :definition :generic
  :element-types :float-complex
  :inputs (x)
  :documentation			; FDL
  "The absolute sum \sum |x_i| of the elements of the vector x.")

(defmfun index-max ((vec vector))
  ("gsl_blas_i" :type "amax")
  (((c-pointer vec) :pointer))
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
  :documentation			; FDL
  "Copy the elements of the vector x into the vector y.")

(defmfun axpy (alpha (x vector) (y vector))
  ("gsl_blas_" :type "axpy")
  ((alpha  :element-c-type) ((mpointer x) :pointer) ((mpointer y) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (x y)
  :outputs (y)
  :documentation			; FDL
  "Copy the elements of the vector x into the vector y.")

(defmfun scale ((alpha :element-type) (x vector))
  ;; Alpha is the same type as the elements of vector, so for complex
  ;; vectors it must be complex.
  ("gsl_blas_" :type "scal")
  ((alpha :element-c-type) ((mpointer x) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (x)
  :outputs (x)
  :documentation			; FDL
  "Rescale the vector x by the multiplicative factor alpha.")

(defmfun scale ((alpha :component-float-type) (x vector))
  ;; Alpha is a float and the vector is complex
  ("gsl_blas_" :type :component-float-type "scal")
  ((alpha :component-float-type) ((mpointer x) :pointer))
  :definition :methods
  :element-types :complex
  :inputs (x)
  :outputs (x))

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

#|
;;; Before expanding, set 
;;; (setf *read-default-float-format* 'double-float)
(make-tests blas1-single
 ;; single
 (letm ((a (vector-single-float #(1.0f0 2.0f0 3.0f0)))
	(b (vector-single-float #(3.0f0 4.0f0 5.0f0))))
   (dot a b))
 (letm ((b (vector-single-float #(3.0f0 4.0f0 5.0f0))))
   (euclidean-norm b))
 (letm ((b (vector-single-float #(3.0f0 4.0f0 5.0f0))))
   (asum b))
 (letm ((b (vector-single-float #(3.0f0 5.0f0 4.0f0))))
   (imax b))
 (letm ((a (vector-single-float #(1.0f0 2.0f0 3.0f0)))
	 (b (vector-single-float #(3.0f0 4.0f0 5.0f0))))
    (setf (data a) #(1.0f0 2.0f0 3.0f0)
	  (data b) #(3.0f0 4.0f0 5.0f0))
    (axpy 2.0f0 a b)
    (data b))
 (letm ((b (vector-single-float #(3.0f0 4.0f0 5.0f0))))
    (setf (data b) #(3.0f0 4.0f0 5.0f0))
    (scal 2.0f0 b)
    (data b))
 (letm ((a (vector-single-float #(1.0f0 3.0f0)))
	 (b (vector-single-float #(8.0f0 9.0f0))))
    (rot a b (/ (sqrt 2.0f0)) (/ (sqrt 2.0f0)))
    (data b)))

;;; Before expanding, set 
;;; (setf *read-default-float-format* 'single-float)
(make-tests blas1-double
 (letm ((a (vector-double-float #(1.0d0 2.0d0 3.0d0)))
	(b (vector-double-float #(3.0d0 4.0d0 5.0d0))))
   (dot a b))
 (letm ((b (vector-double-float #(3.0d0 4.0d0 5.0d0))))
   (euclidean-norm b))
 (letm ((b (vector-double-float #(3.0d0 4.0d0 5.0d0))))
   (setf (data b) #(3.0d0 4.0d0 5.0d0))
   (asum b))
 (letm ((b (vector-double-float #(3.0d0 5.0d0 4.0d0))))
   (setf (data b) #(3.0d0 5.0d0 4.0d0))
   (imax b))
 (letm ((a (vector-double-float #(1.0d0 2.0d0 3.0d0)))
	(b (vector-double-float #(3.0d0 4.0d0 5.0d0))))
   (blas-swap a b)
   (data a))
 (letm ((a (vector-double-float #(1.0d0 2.0d0 3.0d0)))
	(b (vector-double-float #(3.0d0 4.0d0 5.0d0))))
   (blas-copy b a)
   (data a))
 (letm ((a (vector-double-float #(1.0d0 2.0d0 3.0d0)))
	(b (vector-double-float #(3.0d0 4.0d0 5.0d0))))
   (axpy 2.0d0 a b)
   (data b))
 (letm ((b (vector-double-float #(3.0d0 4.0d0 5.0d0))))
   (scal 2.0d0 b)
   (data b))
 (letm ((a (vector-double-float #(1.0d0 3.0d0)))
	(b (vector-double-float #(8.0d0 9.0d0))))
   (rot a b (/ (sqrt 2.0d0)) (/ (sqrt 2.0d0)))
   (data b)))

(LISP-UNIT:DEFINE-TEST BLAS1-SINGLE
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 26.0f0)
   (MULTIPLE-VALUE-LIST
    (LETM ((A (VECTOR-SINGLE-FLOAT #(1.0f0 2.0f0 3.0f0)))
	   (B (VECTOR-SINGLE-FLOAT #(3.0f0 4.0f0 5.0f0))))
      (DOT A B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 7.071068f0)
   (MULTIPLE-VALUE-LIST
    (LETM ((B (VECTOR-SINGLE-FLOAT #(3.0f0 4.0f0 5.0f0))))
      (EUCLIDEAN-NORM B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 12.0f0)
   (MULTIPLE-VALUE-LIST
    (LETM ((B (VECTOR-SINGLE-FLOAT #(3.0f0 4.0f0 5.0f0))))
      (ASUM B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1)
   (MULTIPLE-VALUE-LIST
    (LETM ((B (VECTOR-SINGLE-FLOAT #(3.0f0 5.0f0 4.0f0))))
      (IMAX B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(5.0f0 8.0f0 11.0f0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((A (VECTOR-SINGLE-FLOAT #(1.0f0 2.0f0 3.0f0)))
	 (B (VECTOR-SINGLE-FLOAT #(3.0f0 4.0f0 5.0f0))))
      (SETF (DATA A)
	    #(1.0f0 2.0f0 3.0f0)
	    (DATA B)
	    #(3.0f0 4.0f0 5.0f0))
      (AXPY 2.0f0 A B) (DATA B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(6.0f0 8.0f0 10.0f0))
   (MULTIPLE-VALUE-LIST
    (LETM ((B (VECTOR-SINGLE-FLOAT #(3.0f0 4.0f0 5.0f0))))
      (SETF (DATA B) #(3.0f0 4.0f0 5.0f0))
      (SCAL 2.0f0 B) (DATA B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(4.9497476f0 4.2426405f0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((A (VECTOR-SINGLE-FLOAT #(1.0f0 3.0f0)))
	 (B (VECTOR-SINGLE-FLOAT #(8.0f0 9.0f0))))
      (ROT A B (/ (SQRT 2.0f0)) (/ (SQRT 2.0f0)))
      (DATA B)))))

(LISP-UNIT:DEFINE-TEST BLAS1-DOUBLE
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 26.0d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((A (VECTOR-DOUBLE-FLOAT #(1.0d0 2.0d0 3.0d0)))
	   (B (VECTOR-DOUBLE-FLOAT #(3.0d0 4.0d0 5.0d0))))
      (DOT A B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 7.0710678118654755d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((B (VECTOR-DOUBLE-FLOAT #(3.0d0 4.0d0 5.0d0))))
      (EUCLIDEAN-NORM B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 12.0d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((B (VECTOR-DOUBLE-FLOAT #(3.0d0 4.0d0 5.0d0))))
      (SETF (DATA B) #(3.0d0 4.0d0 5.0d0))
      (ASUM B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1)
   (MULTIPLE-VALUE-LIST
    (LETM ((B (VECTOR-DOUBLE-FLOAT #(3.0d0 5.0d0 4.0d0))))
      (SETF (DATA B) #(3.0d0 5.0d0 4.0d0))
      (IMAX B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(3.0d0 4.0d0 5.0d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((A (VECTOR-DOUBLE-FLOAT #(1.0d0 2.0d0 3.0d0)))
	 (B (VECTOR-DOUBLE-FLOAT #(3.0d0 4.0d0 5.0d0))))
      (BLAS-SWAP A B) (DATA A))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(3.0d0 4.0d0 5.0d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((A (VECTOR-DOUBLE-FLOAT #(1.0d0 2.0d0 3.0d0)))
	   (B (VECTOR-DOUBLE-FLOAT #(3.0d0 4.0d0 5.0d0))))
      (BLAS-COPY B A) (DATA A))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(5.0d0 8.0d0 11.0d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((A (VECTOR-DOUBLE-FLOAT #(1.0d0 2.0d0 3.0d0)))
	   (B (VECTOR-DOUBLE-FLOAT #(3.0d0 4.0d0 5.0d0))))
      (AXPY 2.0d0 A B) (DATA B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(6.0d0 8.0d0 10.0d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((B (VECTOR-DOUBLE-FLOAT #(3.0d0 4.0d0 5.0d0))))
      (SCAL 2.0d0 B) (DATA B))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(4.949747468305832d0 4.242640687119286d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((A (VECTOR-DOUBLE-FLOAT #(1.0d0 3.0d0)))
	   (B (VECTOR-DOUBLE-FLOAT #(8.0d0 9.0d0))))
      (ROT A B (/ (SQRT 2.0d0)) (/ (SQRT 2.0d0)))
      (DATA B)))))
|#

