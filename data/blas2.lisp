;********************************************************
; file:        blas2.lisp                                
; description: BLAS level 2, Matrix-vector operations
; date:        Wed Apr 26 2006 - 21:08                   
; author:      Liam M. Healy                             
; modified:    Wed Apr 26 2006 - 22:43
;********************************************************
;;; $Id: $

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
  (:documentation "The matrix-vector product and sum
    y = \alpha op(A) x + \beta y, where op(A) = A, A^T, A^H
    for TransA = :notrans, :trans, :conjtrans.")
  (:method :after (TransA alpha A x beta y)
	   (cl-invalidate y)))

(defgeneric trmv (uplo TransA diag A x)
  (:documentation "The matrix-vector product x =\alpha op(A) x
   for the triangular matrix A, where op(A) = A, A^T, A^H for
   TransA = :NoTrans, :Trans, :ConjTrans. When Uplo
   is :Upper then the upper triangle of A is used, and when
   Uplo is :Lower then the lower triangle of A is used. If
   Diag is :NonUnit then the diagonal of the matrix is used,
   but if Diag is :Unit then the diagonal elements of the
   matrix A are taken as unity and are not referenced.")
  (:method :after (uplo TransA diag A x)
	   (cl-invalidate x)))

(defgeneric trsv (uplo TransA diag A x)
  (:documentation
   "Compute inv(op(A)) x for x, where op(A) = A, A^T, A^H for
  TransA = :NoTrans, :Trans, :ConjTrans. When Uplo is
  :Upper then the upper triangle of A is used, and when Uplo is
  :Lower then the lower triangle of A is used. If Diag is
  :NonUnit then the diagonal of the matrix is used, but if Diag
  is :Unit then the diagonal elements of the matrix A are taken
  as unity and are not referenced.")
  (:method :after (uplo TransA diag A x)
	   (cl-invalidate x)))

(defgeneric symv (uplo alpha A x beta y)
  (:documentation "The matrix-vector product and sum y = \alpha A
  x + \beta y for the symmetric matrix A. Since the matrix A is
  symmetric only its upper half or lower half need to be
  stored. When Uplo is :Upper then the upper triangle and
  diagonal of A are used, and when Uplo is :Lower then the
  lower triangle and diagonal of A are used.")
  (:method :after (uplo alpha A x beta y)
	   (cl-invalidate y)))

(defgeneric ger (alpha x y A)
  (:documentation "The rank-1 update A = \alpha x y^T + A of the matrix A.")
  (:method :after (alpha x y A)
	   (cl-invalidate A)))

(defgeneric syr (uplo alpha x A)
  (:documentation "The symmetric rank-1 update A = \alpha x x^T +
  A of the symmetric matrix A. Since the matrix A is symmetric
  only its upper half or lower half need to be stored. When Uplo
  is :Upper then the upper triangle and diagonal of A are
  used, and when Uplo is :Lower then the lower triangle and
  diagonal of A are used.")
  (:method :after (uplo alpha x A)
	   (cl-invalidate A)))

(defgeneric syr2 (uplo alpha x y A)
  (:documentation "The symmetric rank-2 update A = \alpha x y^T +
  \alpha y x^T + A of the symmetric matrix A. Since the matrix A
  is symmetric only its upper half or lower half need to be
  stored. When Uplo is :Upper then the upper triangle and
  diagonal of A are used, and when Uplo is :Lower then the
  lower triangle and diagonal of A are used.")
  (:method :after (uplo alpha x y A)
	   (cl-invalidate A)))

;;;;****************************************************************************
;;;; Double
;;;;****************************************************************************

(defun-gsl gemv
    ((transa cblas-transpose) (alpha :double) (A gsl-matrix-c)
     (x gsl-vector-c) (beta :double) (y gsl-vector-c))
  "gsl_blas_dgemv"
  :method
  (TransA alpha (A gsl-matrix) (x gsl-vector) beta (y gsl-vector)))

(defun-gsl trmv
    ((uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
     (A gsl-matrix-c) (x gsl-vector-c))
  "gsl_blas_dtrmv"
  :method
  (uplo transa diag (A gsl-matrix) (x gsl-vector)))

(defun-gsl trsv
    ((uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
     (A gsl-matrix-c) (x gsl-vector-c))
  "gsl_blas_dtrsv"
  :method
  (uplo transa diag (A gsl-matrix) (x gsl-vector)))

(defun-gsl symv
    ((uplo cblas-uplo) (alpha :double) (A gsl-matrix-c)
     (x gsl-vector-c) (beta :double) (y gsl-vector-c))
  "gsl_blas_dsymv"
  :method
  (uplo alpha (A gsl-matrix) (x gsl-vector) beta (y gsl-vector)))

(defun-gsl ger
    ((alpha :double) (x gsl-vector-c) (y gsl-vector-c) (A gsl-matrix-c))
  "gsl_blas_dger"
  :method
  (alpha (x gsl-vector) (y gsl-vector) (A gsl-matrix)))

(defun-gsl syr
    ((uplo cblas-uplo) (alpha :double) (x gsl-vector-c) (A gsl-matrix-c))
  "gsl_blas_dsyr"
  :method
  (uplo alpha (x gsl-vector) (A gsl-matrix)))

(defun-gsl syr2
    ((uplo cblas-uplo) (alpha :double)
     (x gsl-vector-c) (y gsl-vector-c) (A gsl-matrix-c))
  "gsl_blas_dsyr2"
  :method
  (uplo alpha (x gsl-vector) (y gsl-vector) (A gsl-matrix)))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

