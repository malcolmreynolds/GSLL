;; BLAS level 2, Matrix-vector operations
;; Liam Healy, Wed Apr 26 2006 - 21:08
;; Time-stamp: <2008-08-06 22:49:49EDT blas2.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Options
;;;;****************************************************************************

(cffi:defcenum cblas-transpose
  "CBLAS_TRANSPOSE from /usr/include/gsl/gsl_cblas.h."
  (:notrans 111) :trans :conjtrans)

(cffi:defcenum cblas-uplo
  "/usr/include/gsl/gsl_cblas.h."
  (:upper 121) :lower)

(cffi:defcenum cblas-diag
  "/usr/include/gsl/gsl_cblas.h."
 (:nonunit 131) :unit)

;;;;****************************************************************************
;;;; Functions
;;;;****************************************************************************

(defmfun matrix-vector-product
    (TransA alpha (A matrix) (x vector) beta (y vector))
  ("gsl_blas_" :type "gemv")
  ((transa cblas-transpose) (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer x) :pointer) (beta :element-c-type) ((mpointer y) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (A x y)
  :outputs (y)
  :documentation			; FDL
  "The matrix-vector product and sum
    y = alpha op(A) x + beta y, where op(A) = A, A^T, A^H
    for TransA = :notrans, :trans, :conjtrans.")

(defmfun matrix-vector-product-triangular
    (uplo TransA diag (A matrix) (x vector))
  ("gsl_blas_" :type "trmv")
  ((uplo cblas-uplo) (TransA cblas-transpose) (diag cblas-diag)
   ((mpointer A) :pointer) ((mpointer x) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (TransA A x)
  :outputs (x)
  :documentation			; FDL
  "The matrix-vector product x =\alpha op(A) x
   for the triangular matrix A, where op(A) = A, A^T, A^H for
   TransA = :NoTrans, :Trans, :ConjTrans. When Uplo
   is :Upper then the upper triangle of A is used, and when
   Uplo is :Lower then the lower triangle of A is used. If
   Diag is :NonUnit then the diagonal of the matrix is used,
   but if Diag is :Unit then the diagonal elements of the
   matrix A are taken as unity and are not referenced.")

(defmfun inverse-matrix-vector-product
    (uplo TransA diag (A matrix) (x vector))
  ("gsl_blas_" :type "trsv")
  ((uplo cblas-uplo) (TransA cblas-transpose) (diag cblas-diag)
   ((mpointer A) :pointer) ((mpointer x) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (A x)
  :outputs (x)
  :documentation			; FDL
  "Compute inv(op(A)) x for x, where op(A) = A, A^T, A^H for
    TransA = :NoTrans, :Trans, :ConjTrans. When Uplo is
    :Upper then the upper triangle of A is used, and when Uplo is
    :Lower then the lower triangle of A is used. If Diag is
    :NonUnit then the diagonal of the matrix is used, but if Diag
    is :Unit then the diagonal elements of the matrix A are taken
    as unity and are not referenced.")

(defmfun matrix-vector-product-symmetric
    (uplo alpha (A matrix) (x vector) beta (y vector))
  ("gsl_blas_" :type "symv")
  ((uplo cblas-uplo) (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer x) :pointer) (beta :element-c-type) ((mpointer y) :pointer))
  :definition :generic
  :element-types :float
  :inputs (A x y)
  :outputs (y)
  :documentation			; FDL
  "The matrix-vector product and sum y = alpha A
  x + beta y for the symmetric matrix A. Since the matrix A is
  symmetric only its upper half or lower half need to be
  stored. When Uplo is :Upper then the upper triangle and
  diagonal of A are used, and when Uplo is :Lower then the
  lower triangle and diagonal of A are used.")

(defmfun matrix-vector-product-hermitian 
  (uplo alpha (A matrix) (x vector) beta (y vector))
  ("gsl_blas_" :type "hemv")
  ((uplo cblas-uplo) (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer x) :pointer) (beta :element-c-type) ((mpointer y) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (A x y)
  :outputs (y)
  :documentation			; FDL
  "The matrix-vector product and sum y = alpha A x + beta y for the
  hermitian matrix A. Since the matrix A is hermitian only its upper
  half or lower half need to be stored. When Uplo is CblasUpper then
  the upper triangle and diagonal of A are used, and when Uplo is
  CblasLower then the lower triangle and diagonal of A are used. The
  imaginary elements of the diagonal are automatically assumed to be
  zero and are not referenced.")

(defmfun rank-1-update (alpha (x vector) (y vector) (A matrix))
  ("gsl_blas_" :type "ger" :suffix)
  ((alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer x) :pointer) ((mpointer y) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (x y A)
  :outputs (A)
  :documentation			; FDL
   "The rank-1 update A = alpha x y^T + A of the matrix A.")

(defmfun conjugate-rank-1-update (alpha (x vector) (y vector) (A matrix))
  ("gsl_blas_" :type "gerc")
  ((alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer x) :pointer) ((mpointer y) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (x y A)
  :outputs (A)
  :documentation			; FDL
   "The conjugate rank-1 update A = alpha x y^H + A of the matrix A.")

(defmfun symmetric-rank-1-update (uplo alpha (x vector) (A matrix))
  ("gsl_blas_" :type "syr")
  ((uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer x) :pointer) ((mpointer A) :pointer))
  :definition :generic
  :element-types :float
  :inputs (x A)
  :outputs (A)
  :documentation			; FDL
  "The symmetric rank-1 update A = \alpha x x^T +
  A of the symmetric matrix A. Since the matrix A is symmetric
  only its upper half or lower half need to be stored. When Uplo
  is :Upper then the upper triangle and diagonal of A are
  used, and when Uplo is :Lower then the lower triangle and
  diagonal of A are used.")

(defmfun hermitian-rank-1-update (uplo alpha (x vector) (A matrix))
  ("gsl_blas_" :type "her")
  ((uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer x) :pointer) ((mpointer A) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (x A)
  :outputs (A)
  :documentation			; FDL
  "Compute the hermitian rank-1 update A = alpha x x^H + A of the
  hermitian matrix A. Since the matrix A is hermitian only its upper
  half or lower half need to be stored. When Uplo is :upper then
  the upper triangle and diagonal of A are used, and when Uplo is
  :lower then the lower triangle and diagonal of A are used. The
  imaginary elements of the diagonal are automatically set to zero.")

(defmfun symmetric-rank-2-update (uplo alpha (x vector) (y vector) (A matrix))
  ("gsl_blas_" :type "syr2")
  ((uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer x) :pointer)  ((mpointer y) :pointer)
   ((mpointer A) :pointer))
  :definition :generic
  :element-types :float
  :inputs (x y A)
  :outputs (A)
  :documentation			; FDL
  "The symmetric rank-2 update A = alpha x y^T +
  alpha y x^T + A of the symmetric matrix A. Since the matrix A
  is symmetric only its upper half or lower half need to be
  stored. When Uplo is :Upper then the upper triangle and
  diagonal of A are used, and when Uplo is :Lower then the
  lower triangle and diagonal of A are used.")

(defmfun hermitian-rank-2-update (uplo alpha (x vector) (y vector) (A matrix))
  ("gsl_blas_" :type "her2")
  ((uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer x) :pointer) ((mpointer A) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (x A)
  :outputs (A)
  :documentation			; FDL
  "The hermitian rank-2 update A = alpha x y^H + alpha^* y x^H A of
  the hermitian matrix A. Since the matrix A is hermitian only its
  upper half or lower half need to be stored. When uplo is :upper
  then the upper triangle and diagonal of A are used, and when uplo is
  :lower then the lower triangle and diagonal of A are used. The
  imaginary elements of the diagonal are automatically set to zero.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

