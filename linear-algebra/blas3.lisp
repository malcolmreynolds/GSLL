;; BLAS level 3, Matrix-matrix operations
;; Liam Healy, Wed Apr 26 2006 - 21:08
;; Time-stamp: <2009-05-03 11:18:02EDT blas3.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_blas.h

;;;;****************************************************************************
;;;; Options
;;;;****************************************************************************

(cffi:defcenum cblas-side
  "/usr/include/gsl/gsl_cblas.h."
  (:left 141) :right)

#+fsbv
(fsbv:defcenum-aux cblas-side)

;;;;****************************************************************************
;;;; Functions
;;;;****************************************************************************

(defmfun matrix-product
    ((A matrix) (B matrix)
     &optional
     (C (make-marray
	 element-type
	 :dimensions (matrix-product-dimensions A B) :initial-element 0))
     (alpha 1) (beta 1) (TransA :notrans) (TransB :notrans))
  ("gsl_blas_" :type "gemm")
  ((TransA cblas-transpose) (TransB cblas-transpose)
   (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer B) :pointer) (beta :element-c-type) ((mpointer C) :pointer))
  :definition :methods
  :element-types #+fsbv :float-complex #-fsbv :float
  :inputs (A B C)
  :outputs (C))

(defmfun matrix-product-symmetric
    ((A matrix) (B matrix)
     &optional
     (C (make-marray element-type :dimensions (matrix-product-dimensions A B)
		     :initial-element 0))
     (alpha 1) (beta 1) (uplo :upper) (side :left))
  ("gsl_blas_" :type "symm")
  ((side cblas-side) (uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer A) :pointer) ((mpointer B) :pointer)
   (beta :element-c-type) ((mpointer C) :pointer)) 
  :definition :methods
  :element-types #+fsbv :float-complex #-fsbv :float
  :inputs (A B C)
  :outputs (C))

#+fsbv
(defmfun matrix-product-hermitian
    ((A matrix) (B matrix)
     &optional
     (C (make-marray element-type :dimensions (matrix-product-dimensions A B)
		     :initial-element 0))
     (alpha 1) (beta 1) (uplo :upper) (side :left))
  ;; This always signals an error because you can't pass a
  ;; struct in CFFI yet.
  ("gsl_blas_" :type "hemm")
  ((side cblas-side) (uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer A) :pointer) ((mpointer B) :pointer)
   (beta :element-c-type) ((mpointer C) :pointer))
  :definition :methods
  :element-types :complex
  :inputs (A B C)
  :outputs (C))

(defmfun matrix-product-triangular
    ((A matrix) (B matrix)
     &optional (alpha 1) (uplo :upper) (TransA :notrans)
     (diag :nonunit) (side :left))
  ("gsl_blas_" :type "trmm")
  ((side cblas-side) (uplo cblas-uplo) (TransA cblas-transpose)
   (diag cblas-diag)
   (alpha :element-c-type) ((mpointer A) :pointer) ((mpointer B) :pointer))
  :definition :methods
  :element-types #+fsbv :float-complex #-fsbv :float
  :inputs (A B)
  :outputs (B))

(defmfun inverse-matrix-product
    ((A matrix) (B matrix)
     &optional (alpha 1) (uplo :upper) (TransA :notrans)
     (diag :nonunit) (side :left))
  ;; This signals an error for complex arguments because you can't pass a
  ;; struct in CFFI yet.
  ("gsl_blas_" :type "trsm")
  ((side cblas-side) (uplo cblas-uplo)
   (TransA cblas-transpose) (diag cblas-diag)
   (alpha :element-c-type) ((mpointer A) :pointer) ((mpointer B) :pointer))
  :definition :methods
  :element-types #+fsbv :float-complex #-fsbv :float
  :inputs (A B)
  :outputs (B))

(defmfun symmetric-rank-1-update
    ((A matrix) (C matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "syrk")
  ((uplo cblas-uplo) (trans cblas-transpose)
   (alpha :element-c-type) ((mpointer A) :pointer)
   (beta :element-c-type) ((mpointer C) :pointer))
  :definition :methods
  :element-types #+fsbv :float-complex #-fsbv :float
  :inputs (A C)
  :outputs (C))

#+fsbv
(defmfun hermitian-rank-1-update
    ((A matrix) (C matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "herk")
  ((uplo cblas-uplo) (trans cblas-transpose)
   (alpha :element-c-type) ((mpointer A) :pointer)
   (beta :element-c-type) ((mpointer C) :pointer))
  :definition :methods
  :element-types :complex
  :inputs (A C)
  :outputs (C))

(defmfun symmetric-rank-2-update
    ((A matrix) (B matrix) (C matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "syr2k")
  ((uplo cblas-uplo) (trans cblas-transpose)
   (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer B) :pointer) (beta :element-c-type)
   ((mpointer C) :pointer))
  :definition :methods
  :element-types :float
  :inputs (A B C)
  :outputs (C))

#+fsbv
(defmfun hermitian-rank-2-update
    ((A matrix) (B matrix) (C matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "her2k")
  ((uplo cblas-uplo) (trans cblas-transpose)
   (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer B) :pointer) (beta :element-c-type)
   ((mpointer C) :pointer))
  :definition :methods
  :element-types :complex
  :inputs (A B C)
  :outputs (C))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(generate-all-array-tests matrix-product #+fsbv :float-complex #-fsbv :float
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3)))
       (m3 (array-default '(3 3)))
       (s1 (scalar-default))
       (s2 (scalar-default)))
   (cl-array (matrix-product m1 m2 m3 s1 s2))))

(generate-all-array-tests matrix-product-nonsquare :float-complex
 (let ((m1 (array-default '(2 3)))
       (m2 (array-default '(3 2))))
   (cl-array (matrix-product m1 m2))))

(generate-all-array-tests matrix-product-triangular
			  #+fsbv :float-complex #-fsbv :float
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3)))
       (s1 (scalar-default)))
   (cl-array (matrix-product-triangular m1 m2 s1))))

(generate-all-array-tests inverse-matrix-product 
			  #+fsbv :float-complex #-fsbv :float
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3)))
       (s1 (scalar-default)))
   (cl-array (inverse-matrix-product m1 m2 s1))))

(generate-all-array-tests matrix-product-symmetric :float
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3)))
       (m3 (array-default '(3 3)))
       (s1 (scalar-default))
       (s2 (scalar-default)))
   (cl-array (matrix-product-symmetric m1 m2 m3 s1 s2))))

#+fsbv
(generate-all-array-tests matrix-product-hermitian :complex
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3)))
       (m3 (array-default '(3 3)))
       (s1 (scalar-default))
       (s2 (scalar-default)))
   (cl-array (matrix-product-hermitian m1 m2 m3 s1 s2))))
