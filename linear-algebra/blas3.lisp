;; BLAS level 3, Matrix-matrix operations
;; Liam Healy, Wed Apr 26 2006 - 21:08
;; Time-stamp: <2008-08-09 19:46:23EDT blas3.lisp>
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
   (A :pointer) (B :pointer) (beta :element-c-type) (C :pointer)) 
  :definition :generic
  :element-types :float-complex
  :inputs (A B C)
  :outputs (C)
  :return (C))

(defmfun matrix-product-hermitian
    ((A matrix) (B matrix) (C matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (side :left))
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
  ("gsl_blas_" :type "trsm")
  ((uplo cblas-uplo) (TransA cblas-transpose) (diag cblas-diag)
   ((mpointer A) :pointer) ((mpointer B) :pointer))
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

