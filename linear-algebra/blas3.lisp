;********************************************************
; file:        blas3.lisp                                
; description: BLAS level 3, Matrix-matrix operations
; date:        Wed Apr 26 2006 - 21:08                   
; author:      Liam M. Healy                             
; modified:    Mon Jul  3 2006 - 23:45
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Options
;;;;****************************************************************************

(cffi:defcenum cblas-side
  "/usr/include/gsl/gsl_cblas.h."
  (:left 141) :right)

;;;;****************************************************************************
;;;; Generic
;;;;****************************************************************************

(export '(gemm symm trmm trsm syrk syr2k))

(defgeneric gemm (TransA TransB alpha A B beta C)
  (:documentation "The matrix-matrix product and sum C = \alpha
  op(A) op(B) + \beta C where op(A) = A, A^T, A^H for TransA =
  :NoTrans, :Trans, :ConjTrans and similarly for the
  parameter TransB.")
  (:method :after (TransA TransB alpha A B beta C)
	   (cl-invalidate C)))

(defgeneric symm (side uplo alpha A B beta C)
  (:documentation "The matrix-matrix product and sum C = \alpha A
  B + \beta C for Side is :Left and C = \alpha B A + \beta C
  for Side is :Right, where the matrix A is symmetric. When
  Uplo is :Upper then the upper triangle and diagonal of A
  are used, and when Uplo is :Lower then the lower triangle
  and diagonal of A are used.")
  (:method :after (side uplo alpha A B beta C)
	   (cl-invalidate C)))

(defgeneric trmm (side uplo TransA diag alpha A B)
  (:documentation "The matrix-matrix product B = \alpha op(A) B
  for Side is :Left and B = \alpha B op(A) for Side is
  :Right. The matrix A is triangular and op(A) = A, A^T, A^H
  for TransA = :NoTrans, :Trans, :ConjTrans When Uplo
  is :Upper then the upper triangle of A is used, and when
  Uplo is :Lower then the lower triangle of A is used. If
  Diag is :NonUnit then the diagonal of A is used, but if
  Diag is :Unit then the diagonal elements of the matrix A
  are taken as unity and are not referenced.")
  (:method :after (side uplo TransA diag alpha A B)
	   (cl-invalidate B)))

(defgeneric trsm (side uplo TransA diag alpha A B)
  (:documentation
   "The inverse-matrix matrix product B = \alpha op(inv(A))B for
   Side is :Left and B = \alpha B op(inv(A)) for Side is
   :Right. The matrix A is triangular and op(A) = A, A^T, A^H
   for TransA = :NoTrans, :Trans, :ConjTrans When
   Uplo is :Upper then the upper triangle of A is used, and
   when Uplo is :Lower then the lower triangle of A is
   used. If Diag is :NonUnit then the diagonal of A is used,
   but if Diag is :Unit then the diagonal elements of the
   matrix A are taken as unity and are not referenced.")
  (:method :after (side uplo TransA diag alpha A B)
	   (cl-invalidate B)))

(defgeneric syrk (uplo trans alpha A beta C)
  (:documentation "A rank-k update of the symmetric matrix C, C =
  \alpha A A^T + \beta C when Trans is :NoTrans and C =
  \alpha A^T A + \beta C when Trans is :Trans. Since the
  matrix C is symmetric only its upper half or lower half need to
  be stored. When Uplo is :Upper then the upper triangle and
  diagonal of C are used, and when Uplo is :Lower then the
  lower triangle and diagonal of C are used.")
  (:method :after (uplo trans alpha A beta C)
	   (cl-invalidate C)))

(defgeneric syr2k (uplo trans alpha A B beta C)
  (:documentation "A rank-2k update of the symmetric matrix C, C
  = \alpha A B^T + \alpha B A^T + \beta C when Trans is
  :NoTrans and C = \alpha A^T B + \alpha B^T A + \beta C when
  Trans is :Trans. Since the matrix C is symmetric only its
  upper half or lower half need to be stored. When Uplo is
  :Upper then the upper triangle and diagonal of C are used,
  and when Uplo is :Lower then the lower triangle and
  diagonal of C are used.")
  (:method :after (uplo trans alpha A B beta C)
	   (cl-invalidate C)))

;;;;****************************************************************************
;;;; Single
;;;;****************************************************************************

(defun-gsl gemm
    (TransA TransB alpha (A gsl-matrix-single) (B gsl-matrix-single)
	    beta (C gsl-matrix-single))
  "gsl_blas_sgemm"
  ((transa cblas-transpose) (transb cblas-transpose)
   (alpha :float) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :float) ((pointer C) gsl-matrix-c))
  :type :method)

(defun-gsl symm
    (side uplo alpha (A gsl-matrix-single) (B gsl-matrix-single)
	  beta (C gsl-matrix-single))
  "gsl_blas_ssymm"
  ((side cblas-side) (uplo cblas-uplo) (alpha :float)
   ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :float) ((pointer C) gsl-matrix-c))
  :type :method)

(defun-gsl trmm
    (side uplo transa diag alpha (A gsl-matrix-single) (B gsl-matrix-single))
  "gsl_blas_strmm"
  ((side cblas-side) (uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   (alpha :float) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c))
  :type :method)

(defun-gsl trsm
    (side uplo transa diag alpha (A gsl-matrix-single) (B gsl-matrix-single))
  "gsl_blas_strsm"
  ((side cblas-side) (uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   (alpha :float) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c))
  :type :method)

(defun-gsl syrk
    (uplo trans alpha (A gsl-matrix-single) beta (C gsl-matrix-single))
  "gsl_blas_ssyrk"
  ((uplo cblas-uplo) (trans cblas-transpose) (alpha :float)
   ((pointer A) gsl-matrix-c) (beta :float) ((pointer C) gsl-matrix-c))
  :type :method)

(defun-gsl syr2k
    (uplo trans alpha (A gsl-matrix-single) (B gsl-matrix-single)
	  beta (C gsl-matrix-single))
  "gsl_blas_ssyr2k"
  ((uplo cblas-uplo) (trans cblas-transpose) (alpha :float)
   ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :float) ((pointer C) gsl-matrix-c))
  :type :method)

;;;;****************************************************************************
;;;; Double
;;;;****************************************************************************

(defun-gsl gemm
    (TransA TransB alpha (A gsl-matrix-double) (B gsl-matrix-double)
	    beta (C gsl-matrix-double))
  "gsl_blas_dgemm"
  ((transa cblas-transpose) (transb cblas-transpose)
   (alpha :double) (A gsl-matrix-c) (B gsl-matrix-c)
   (beta :double) (C gsl-matrix-c))
  :type :method)

(defun-gsl symm
    (side uplo alpha (A gsl-matrix-double) (B gsl-matrix-double)
	  beta (C gsl-matrix-double))
  "gsl_blas_dsymm"
  ((side cblas-side) (uplo cblas-uplo) (alpha :double)
   ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :double) ((pointer C) gsl-matrix-c))
  :type :method)

(defun-gsl trmm
    (side uplo transa diag alpha (A gsl-matrix-double) (B gsl-matrix-double))
  "gsl_blas_dtrmm"
  ((side cblas-side) (uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   (alpha :double) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c))
  :type :method)

(defun-gsl trsm
    (side uplo transa diag alpha (A gsl-matrix-double) (B gsl-matrix-double))
  "gsl_blas_dtrsm"
  ((side cblas-side) (uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   (alpha :double) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c))
  :type :method)

(defun-gsl syrk
    (uplo trans alpha (A gsl-matrix-double) beta (C gsl-matrix-double))
  "gsl_blas_dsyrk"
  ((uplo cblas-uplo) (trans cblas-transpose) (alpha :double)
   ((pointer A) gsl-matrix-c) (alpha :double) ((pointer C) gsl-matrix-c))
  :type :method)

(defun-gsl syr2k
    (uplo trans (alpha double-float) (A gsl-matrix-double) (B gsl-matrix-double)
	  beta (C gsl-matrix-double))
  "gsl_blas_dsyr2k"
  ((uplo cblas-uplo) (trans cblas-transpose) (alpha :double)
   ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :double) ((pointer C) gsl-matrix-c))
  :type :method)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

