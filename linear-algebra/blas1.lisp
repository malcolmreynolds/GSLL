;; BLAS level 1, Vector operations
;; Liam Healy, Wed Apr 26 2006 - 15:23
;; Time-stamp: <2008-08-02 19:19:02EDT blas1.lisp>
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

(defmfun 2norm ((vec1 vector) (vec2 vector))
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

(defmfun scale ((alpha :element-c-type) (x vector))
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


;;;;;;;;;;;;;;;;; OLD ;;;;;;;;;;;;;;;;;;;;

;;; Currently only includes vector-single-float and vector-double-float routines.
;;; Not ported: routines that use raw vectors gsl_blas_drotg, gsl_blas_drotmg, gsl_blas_drotm

;;;;****************************************************************************
;;;; Generic
;;;;****************************************************************************

(export '(dot norm asum imax swap copy axpy scal rot))


(defgeneric rot (x y c s)
  (:documentation			; FDL
   "Apply a Givens rotation (x', y') = (c x + s y, -s x + c y) to the vectors x, y."))

;;;;****************************************************************************
;;;; Single
;;;;****************************************************************************

(defmfun sdot (result alpha vec1 vec2)
  "gsl_blas_sdsdot"
  ((alpha :float) ((pointer vec1) :pointer) ((pointer vec2) :pointer)
   ((gsl-array result) :pointer))
  :invalidate (result))

(defmfun dot ((vec1 vector-single-float) (vec2 vector-single-float))
  "gsl_blas_sdot"
  (((pointer vec1) :pointer) ((pointer vec2) :pointer) (result :float))
  :type :method)

(defmfun norm ((vec vector-single-float))
  "gsl_blas_snrm2"  (((pointer vec) :pointer))
  :type :method
  :c-return :float)

(defmfun asum ((vec vector-single-float))
  "gsl_blas_sasum" (((pointer vec) :pointer))
  :type :method
  :c-return :float)

(defmfun imax ((vec vector-single-float))
  "gsl_blas_isamax" (((pointer vec) :pointer))
  :type :method 
  :c-return :int)

(defmfun blas-swap ((vec1 vector-single-float) (vec2 vector-single-float))
  "gsl_blas_sswap" (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method 
  :invalidate (vec1 vec2))

(defmfun blas-copy ((vec1 vector-single-float) (vec2 vector-single-float))
  "gsl_blas_scopy" (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method
  :invalidate (vec2))

(defmfun axpy (alpha (vec1 vector-single-float) (vec2 vector-single-float))
  "gsl_blas_saxpy"
  ((alpha :float) ((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method 
  :invalidate (vec2))

(defmfun scal (alpha (vec vector-single-float))
  "gsl_blas_sscal" ((alpha :float) ((pointer vec) :pointer))
  :type :method 
  :invalidate (vec)
  :c-return :void)

(defmfun rot
    ((vec1 vector-single-float) (vec2 vector-single-float)
     (c float) (s float))
  "gsl_blas_srot"
  (((pointer vec1) :pointer) ((pointer vec2) :pointer) (c :float) (s :float))
  :type :method
  :invalidate (vec1 vec2))

;;;;****************************************************************************
;;;; Double
;;;;****************************************************************************

(defmfun dot ((vec1 vector-double-float) (vec2 vector-double-float))
  "gsl_blas_ddot"
  (((pointer vec1) :pointer) ((pointer vec2) :pointer) (result :double))
  :type :method)

(defmfun norm ((vec vector-double-float))
  "gsl_blas_dnrm2"  (((pointer vec) :pointer))
  :type :method
  :c-return :double)

(defmfun asum ((vec vector-double-float))
  "gsl_blas_dasum" (((pointer vec) :pointer))
  :type :method
  :c-return :double)

(defmfun imax ((vec vector-double-float))
  "gsl_blas_idamax" (((pointer vec) :pointer))
  :type :method 
  :c-return :int)

(defmfun blas-swap ((vec1 vector-double-float) (vec2 vector-double-float))
  "gsl_blas_dswap" (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method 
  :invalidate (vec1 vec2))

(defmfun blas-copy ((vec1 vector-double-float) (vec2 vector-double-float))
  "gsl_blas_dcopy" (((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method
  :invalidate (vec2))

(defmfun axpy (alpha (vec1 vector-double-float) (vec2 vector-double-float))
  "gsl_blas_daxpy"
  ((alpha :double) ((pointer vec1) :pointer) ((pointer vec2) :pointer))
  :type :method 
  :invalidate (vec2))

(defmfun scal (alpha (vec vector-double-float))
  "gsl_blas_dscal" ((alpha :double) ((pointer vec) :pointer))
  :type :method 
  :invalidate (vec)
  :c-return :void)

(defmfun rot
    ((vec1 vector-double-float) (vec2 vector-double-float)
     (c float) (s float))
  "gsl_blas_drot"
  (((pointer vec1) :pointer) ((pointer vec2) :pointer) (c :double) (s :double))
  :type :method
  :invalidate (vec1 vec2))

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
   (norm b))
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
   (norm b))
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

|#

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
      (NORM B))))
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
      (NORM B))))
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


