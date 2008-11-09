;; BLAS level 3, Matrix-matrix operations
;; Liam Healy, Wed Apr 26 2006 - 21:08
;; Time-stamp: <2008-11-09 18:12:05EST blas3.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Options
;;;;****************************************************************************

(cffi:defcenum cblas-side
  "/usr/include/gsl/gsl_cblas.h."
  (:left 141) :right)

;;;;****************************************************************************
;;;; Functions
;;;;****************************************************************************

(defmfun matrix-product
    ((A matrix) (B matrix) (C matrix)
     &optional (alpha 1) (beta 1) (TransA :notrans) (TransB :notrans))
  ("gsl_blas_" :type "gemm")
  ((TransA cblas-transpose) (TransB cblas-transpose)
   (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer B) :pointer) (beta :element-c-type) ((mpointer C) :pointer))
  :definition :methods
  :element-types :float-complex
  :inputs (A B C)
  :outputs (C)
  :return (C))

(defmfun matrix-product-symmetric
    ((A matrix) (B matrix) (C matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (side :left))
  ("gsl_blas_" :type "symm")
  ((side cblas-side) (uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer A) :pointer) ((mpointer B) :pointer)
   (beta :element-c-type) ((mpointer C) :pointer)) 
  :definition :methods
  :element-types :float-complex
  :inputs (A B C)
  :outputs (C)
  :return (C))

(defmfun matrix-product-hermitian
    ((A matrix) (B matrix) (C matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (side :left))
  ;; This always signals an error because you can't pass a
  ;; struct in CFFI yet.
  ("gsl_blas_" :type "hemm")
  ((side cblas-side) (uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer A) :pointer) ((mpointer B) :pointer)
   (beta :element-c-type) ((mpointer C) :pointer))
  :definition :methods
  :element-types :complex
  :inputs (A B C)
  :outputs (C)
  :return (C))

(defmfun matrix-product-triangular
    ((A matrix) (B matrix)
     &optional (alpha 1) (uplo :upper) (TransA :notrans)
     (diag :nonunit) (side :left))
  ("gsl_blas_" :type "trmm")
  ((side cblas-side) (uplo cblas-uplo) (TransA cblas-transpose)
   (diag cblas-diag)
   (alpha :element-c-type) ((mpointer A) :pointer) ((mpointer B) :pointer))
  :definition :methods
  :element-types :float-complex
  :inputs (A B)
  :outputs (B)
  :return (B))

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
  :element-types :float-complex
  :inputs (A B)
  :outputs (B)
  :return (B))

(defmfun symmetric-rank-1-update
    ((A matrix) (C matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "syrk")
  ((uplo cblas-uplo) (trans cblas-transpose)
   (alpha :element-c-type) ((mpointer A) :pointer)
   (beta :element-c-type) ((mpointer C) :pointer))
  :definition :methods
  :element-types :float-complex
  :inputs (A C)
  :outputs (C)
  :return (C))

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
  :outputs (C)
  :return (C))

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
  :outputs (C)
  :return (C))

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
  :outputs (C)
  :return (C))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(generate-all-array-tests matrix-product :float-complex
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3)))
	(answer (array-default '(3 3) t))
	(s1 (scalar-default))
	(s2 (scalar-default)))
   (cl-array (matrix-product m1 m2 answer s1 s2))))

(generate-all-array-tests matrix-product-triangular :float-complex
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3)))
	(s1 (scalar-default)))
   (cl-array (matrix-product-triangular m1 m2 s1))))

(generate-all-array-tests inverse-matrix-product :float-complex
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3)))
	(s1 (scalar-default)))
   (cl-array (inverse-matrix-product m1 m2 s1))))

(generate-all-array-tests matrix-product-symmetric :float
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3)))
	(answer (array-default '(3 3) t))
	(s1 (scalar-default))
	(s2 (scalar-default)))
   (cl-array (matrix-product-symmetric m1 m2 answer s1 s2))))

(generate-all-array-tests matrix-product-hermitian :complex
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3)))
	(answer (array-default '(3 3) t))
	(s1 (scalar-default))
	(s2 (scalar-default)))
   (cl-array (matrix-product-hermitian m1 m2 answer s1 s2))))
