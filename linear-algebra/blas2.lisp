;; BLAS level 2, Matrix-vector operations
;; Liam Healy, Wed Apr 26 2006 - 21:08
;; Time-stamp: <2008-02-17 10:36:36EST blas2.lisp>
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
;;;; Generic
;;;;****************************************************************************

(export '(gemv trmv trsv symv ger syr syr2))

(defgeneric gemv (TransA alpha A x beta y)
  (:method :after (TransA alpha A x beta y)
	   (cl-invalidate y))
  (:documentation			; FDL
   "The matrix-vector product and sum
    y = alpha op(A) x + beta y, where op(A) = A, A^T, A^H
    for TransA = :notrans, :trans, :conjtrans."))

(defgeneric trmv (uplo TransA diag A x)
  (:method :after (uplo TransA diag A x)
	   (cl-invalidate x))
  (:documentation			; FDL
   "The matrix-vector product x =\alpha op(A) x
   for the triangular matrix A, where op(A) = A, A^T, A^H for
   TransA = :NoTrans, :Trans, :ConjTrans. When Uplo
   is :Upper then the upper triangle of A is used, and when
   Uplo is :Lower then the lower triangle of A is used. If
   Diag is :NonUnit then the diagonal of the matrix is used,
   but if Diag is :Unit then the diagonal elements of the
   matrix A are taken as unity and are not referenced."))

(defgeneric trsv (uplo TransA diag A x)
  (:method :after (uplo TransA diag A x)
	   (cl-invalidate x))
  (:documentation			; FDL
   "Compute inv(op(A)) x for x, where op(A) = A, A^T, A^H for
    TransA = :NoTrans, :Trans, :ConjTrans. When Uplo is
    :Upper then the upper triangle of A is used, and when Uplo is
    :Lower then the lower triangle of A is used. If Diag is
    :NonUnit then the diagonal of the matrix is used, but if Diag
    is :Unit then the diagonal elements of the matrix A are taken
    as unity and are not referenced."))

(defgeneric symv (uplo alpha A x beta y)
  (:method :after (uplo alpha A x beta y)
	   (cl-invalidate y))
  (:documentation			; FDL
   "The matrix-vector product and sum y = alpha A
  x + beta y for the symmetric matrix A. Since the matrix A is
  symmetric only its upper half or lower half need to be
  stored. When Uplo is :Upper then the upper triangle and
  diagonal of A are used, and when Uplo is :Lower then the
  lower triangle and diagonal of A are used."))

(defgeneric ger (alpha x y A)
  (:method :after (alpha x y A)
	   (cl-invalidate A))
  (:documentation			; FDL
   "The rank-1 update A = alpha x y^T + A of the matrix A."))

(defgeneric syr (uplo alpha x A)
  (:method :after (uplo alpha x A)
	   (cl-invalidate A))
  (:documentation			; FDL
   "The symmetric rank-1 update A = \alpha x x^T +
  A of the symmetric matrix A. Since the matrix A is symmetric
  only its upper half or lower half need to be stored. When Uplo
  is :Upper then the upper triangle and diagonal of A are
  used, and when Uplo is :Lower then the lower triangle and
  diagonal of A are used."))

(defgeneric syr2 (uplo alpha x y A)
  (:method :after (uplo alpha x y A)
	   (cl-invalidate A))
  (:documentation			; FDL
   "The symmetric rank-2 update A = \alpha x y^T +
  alpha y x^T + A of the symmetric matrix A. Since the matrix A
  is symmetric only its upper half or lower half need to be
  stored. When Uplo is :Upper then the upper triangle and
  diagonal of A are used, and when Uplo is :Lower then the
  lower triangle and diagonal of A are used."))

;;;;****************************************************************************
;;;; Single
;;;;****************************************************************************

(defmfun gemv
    (TransA alpha
	    (A gsl-matrix-single) (x gsl-vector-single) beta (y gsl-vector-single))
  "gsl_blas_sgemv"
  ((transa cblas-transpose) (alpha :float) ((pointer A) gsl-matrix-c)
   ((pointer x) gsl-vector-c) (beta :float) ((pointer y) gsl-vector-c))
  :type :method)

(defmfun trmv (uplo transa diag (A gsl-matrix-single) (x gsl-vector-single))
  "gsl_blas_strmv"
  ((uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   ((pointer A) gsl-matrix-c) ((pointer x) gsl-vector-c))
  :type :method)

(defmfun trsv (uplo transa diag (A gsl-matrix-single) (x gsl-vector-single))
  "gsl_blas_strsv"
  ((uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   ((pointer A) gsl-matrix-c) ((pointer x) gsl-vector-c))
  :type :method)

(defmfun symv
    (uplo alpha (A gsl-matrix-single) (x gsl-vector-single)
	  beta (y gsl-vector-single))
  "gsl_blas_ssymv"
  ((uplo cblas-uplo) (alpha :float) ((pointer A) gsl-matrix-c)
   ((pointer x) gsl-vector-c) (beta :float) ((pointer y) gsl-vector-c))
  :type :method)

(defmfun ger
    (alpha (x gsl-vector-single) (y gsl-vector-single) (A gsl-matrix-single))
  "gsl_blas_sger"
  ((alpha :float) ((pointer x) gsl-vector-c)
   ((pointer y) gsl-vector-c) ((pointer A) gsl-matrix-c))
  :type :method)

(defmfun syr (uplo alpha (x gsl-vector-single) (A gsl-matrix-single))
  "gsl_blas_ssyr"
  ((uplo cblas-uplo) (alpha :float)
   ((pointer x) gsl-vector-c) ((pointer A) gsl-matrix-c))
  :type :method)

(defmfun syr2
    (uplo alpha (x gsl-vector-single) (y gsl-vector-single) (A gsl-matrix-single))
  "gsl_blas_ssyr2"
  ((uplo cblas-uplo) (alpha :float)
   ((pointer x) gsl-vector-c) ((pointer y) gsl-vector-c)
   ((pointer A) gsl-matrix-c))
  :type :method)

;;;;****************************************************************************
;;;; Double
;;;;****************************************************************************

(defmfun gemv
    (TransA alpha
	    (A gsl-matrix-double) (x gsl-vector-double) beta (y gsl-vector-double))
  "gsl_blas_dgemv"
  ((transa cblas-transpose) (alpha :double) ((pointer A) gsl-matrix-c)
   ((pointer x) gsl-vector-c) (beta :double) ((pointer y) gsl-vector-c))
  :type :method)

(defmfun trmv (uplo transa diag (A gsl-matrix-double) (x gsl-vector-double))
  "gsl_blas_dtrmv"
  ((uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   ((pointer A) gsl-matrix-c) ((pointer x) gsl-vector-c))
  :type :method)

(defmfun trsv (uplo transa diag (A gsl-matrix-double) (x gsl-vector-double))
  "gsl_blas_dtrsv"
  ((uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   ((pointer A) gsl-matrix-c) ((pointer x) gsl-vector-c))
  :type :method)

(defmfun symv
    (uplo alpha (A gsl-matrix-double) (x gsl-vector-double)
	  beta (y gsl-vector-double))
  "gsl_blas_dsymv"
  ((uplo cblas-uplo) (alpha :double) ((pointer A) gsl-matrix-c)
   ((pointer x) gsl-vector-c) (beta :double) ((pointer y) gsl-vector-c))
  :type :method)

(defmfun ger
    (alpha (x gsl-vector-double) (y gsl-vector-double) (A gsl-matrix-double))
  "gsl_blas_dger"
  ((alpha :double) ((pointer x) gsl-vector-c)
   ((pointer y) gsl-vector-c) ((pointer A) gsl-matrix-c))
  :type :method)

(defmfun syr (uplo alpha (x gsl-vector-double) (A gsl-matrix-double))
  "gsl_blas_dsyr"
  ((uplo cblas-uplo) (alpha :double)
   ((pointer x) gsl-vector-c) ((pointer A) gsl-matrix-c))
  :type :method)

(defmfun syr2
    (uplo alpha (x gsl-vector-double) (y gsl-vector-double) (A gsl-matrix-double))
  "gsl_blas_dsyr2"
  ((uplo cblas-uplo) (alpha :double)
   ((pointer x) gsl-vector-c) ((pointer y) gsl-vector-c)
   ((pointer A) gsl-matrix-c))
  :type :method)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

