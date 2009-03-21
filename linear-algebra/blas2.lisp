;; BLAS level 2, Matrix-vector operations
;; Liam Healy, Wed Apr 26 2006 - 21:08
;; Time-stamp: <2009-03-15 17:09:00EDT blas2.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_blas.h

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

(defun matrix-product-dimensions (a b)
  (if (typep b 'matrix)
      (list (first (dimensions a))
	    (second (dimensions b)))
      (first (dimensions a))))

;;; To do: the y should be an optional argument, default to a zero vector.
(defmfun matrix-product
    ((A matrix) (x vector)
     &optional
     (y (make-marray element-type :dimensions (matrix-product-dimensions A x)))
     (alpha 1) (beta 1) (TransA :notrans) TransB)
  ("gsl_blas_" :type "gemv")
  ((transa cblas-transpose) (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer x) :pointer) (beta :element-c-type) ((mpointer y) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (A x y)
  :outputs (y)
  :documentation			; FDL
  "If the second and third arguments are vectors, compute
   the matrix-vector product and sum
    y = alpha op(A) x + beta y, where op(A) = A, A^T, A^H
  for TransA = :notrans, :trans, :conjtrans.
  If the second and third arguments are matrices, compute
  the matrix-matrix product and sum C = alpha
  op(A) op(B) + beta C where op(A) = A, A^T, A^H for TransA =
  :notrans, :trans, :conjtrans and similarly for the
  parameter TransB.")

(defmfun matrix-product-triangular
    ((A matrix) (x vector)
     &optional (alpha 1) (uplo :upper) (TransA :notrans)
     (diag :nonunit) (side :left))
  ("gsl_blas_" :type "trmv")
  ((uplo cblas-uplo) (TransA cblas-transpose) (diag cblas-diag)
   ((mpointer A) :pointer) ((mpointer x) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (A x)
  :outputs (x)
  :documentation			; FDL
  "If the second argument is a vector, compute
   the matrix-vector product x =alpha op(A) x
   for the triangular matrix A, where op(A) = A, A^T, A^H for
   TransA = :NoTrans, :Trans, :ConjTrans. When Uplo
   is :Upper then the upper triangle of A is used, and when
   Uplo is :Lower then the lower triangle of A is used. If
   Diag is :NonUnit then the diagonal of the matrix is used,
   but if Diag is :Unit then the diagonal elements of the
   matrix A are taken as unity and are not referenced.
   If the second argument is a matrix, compute
   the matrix-matrix product B = alpha op(A) B
   if Side is :Left and B = \alpha B op(A) if Side is
   :Right. The matrix A is triangular and op(A) = A, A^T, A^H
   for TransA = :NoTrans, :Trans, :ConjTrans When Uplo
   is :Upper then the upper triangle of A is used, and when
   Uplo is :Lower then the lower triangle of A is used. If
   Diag is :NonUnit then the diagonal of A is used, but if
   Diag is :Unit then the diagonal elements of the matrix A
   are taken as unity and are not referenced.")

(defmfun inverse-matrix-product
    ((A matrix) (x vector)
     &optional (alpha 1) (uplo :upper) (TransA :notrans)
     (diag :nonunit) (side :left))
  ("gsl_blas_" :type "trsv")
  ((uplo cblas-uplo) (TransA cblas-transpose) (diag cblas-diag)
   ((mpointer A) :pointer) ((mpointer x) :pointer))
  :definition :generic
  :element-types :float-complex
  :inputs (A x)
  :outputs (x)
  :documentation			; FDL
   "If the second argument is a vector, compute
   inv(op(A)) x for x, where op(A) = A, A^T, A^H for
   TransA = :NoTrans, :Trans, :ConjTrans. When Uplo is
   :Upper then the upper triangle of A is used, and when Uplo is
   :Lower then the lower triangle of A is used. If Diag is
   :NonUnit then the diagonal of the matrix is used, but if Diag
   is :Unit then the diagonal elements of the matrix A are taken
   as unity and are not referenced.
   If the second argument is a matrix, compute
   the inverse-matrix matrix product B = alpha op(inv(A))B if
   Side is :Left and B = alpha B op(inv(A)) if Side is
   :Right. The matrix A is triangular and op(A) = A, A^T, A^H
   for TransA = :NoTrans, :Trans, :ConjTrans When
   Uplo is :Upper then the upper triangle of A is used, and
   when Uplo is :Lower then the lower triangle of A is
   used. If Diag is :NonUnit then the diagonal of A is used,
   but if Diag is :Unit then the diagonal elements of the
   matrix A are taken as unity and are not referenced.")

(defmfun matrix-product-symmetric
    ((A matrix) (x vector)
     &optional
     (y (make-marray element-type :dimensions (matrix-product-dimensions A x)))
     (alpha 1) (beta 1) (uplo :upper) (side :left))
  ("gsl_blas_" :type "symv")
  ((uplo cblas-uplo) (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer x) :pointer) (beta :element-c-type) ((mpointer y) :pointer))
  :definition :generic
  :element-types :float
  :inputs (A x y)
  :outputs (y)
  :documentation			; FDL
  "If the second and third arguments are vectors, compute
  the matrix-vector product and sum y = alpha A
  x + beta y for the symmetric matrix A. Since the matrix A is
  symmetric only its upper half or lower half need to be
  stored. When Uplo is :Upper then the upper triangle and
  diagonal of A are used, and when Uplo is :Lower then the
  lower triangle and diagonal of A are used.
  If the second and third arguments are matrices, compute
  the matrix-matrix product and sum C = alpha A
  B + beta C for Side is :Left and C = alpha B A + beta C
  for Side is :Right, where the matrix A is symmetric. When
  Uplo is :Upper then the upper triangle and diagonal of A
  are used, and when Uplo is :Lower then the lower triangle
  and diagonal of A are used.")

(defmfun matrix-product-hermitian
    ((A matrix) (x vector)
     &optional
     (y (make-marray element-type :dimensions (matrix-product-dimensions A x)))
     (alpha 1) (beta 1) (uplo :upper) (side :left))
  ;; This always signals an error because you can't pass a
  ;; struct in CFFI yet.
  ("gsl_blas_" :type "hemv")
  ((uplo cblas-uplo) (alpha :element-c-type) ((mpointer A) :pointer)
   ((mpointer x) :pointer) (beta :element-c-type) ((mpointer y) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (A x y)
  :outputs (y)
  :documentation			; FDL
  "If the second and third arguments are vectors, compute the
  matrix-vector product and sum y = alpha A x + beta y for the
  hermitian matrix A. Since the matrix A is hermitian only its upper
  half or lower half need to be stored. When Uplo is :upper then
  the upper triangle and diagonal of A are used, and when Uplo is
  :lower then the lower triangle and diagonal of A are used. The
  imaginary elements of the diagonal are automatically assumed to be
  zero and are not referenced.  If the second and third arguments are
  matrices, compute the matrix-matrix product and sum C = alpha A B +
  beta C if Side is :left and C = \alpha B A + \beta C if Side
  is :right, where the matrix A is hermitian. When Uplo is
  :upper then the upper triangle and diagonal of A are used, and
  when Uplo is :lower then the lower triangle and diagonal of A
  are used. The imaginary elements of the diagonal are automatically
  set to zero.")

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

(defmfun symmetric-rank-1-update
    ((x vector) (A matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "syr")
  ((uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer x) :pointer) ((mpointer A) :pointer))
  :definition :generic
  :element-types :float
  :inputs (x A)
  :outputs (A)
  :documentation			; FDL
  "If the first argument is a vector,
  the symmetric rank-1 update A = \alpha x x^T + A of the symmetric
  matrix A. Since the matrix A is symmetric only its upper half or
  lower half need to be stored. When Uplo is :Upper then the upper
  triangle and diagonal of A are used, and when Uplo is :Lower then
  the lower triangle and diagonal of A are used.  If the first
  argument is a matrix, a rank-k update of the symmetric matrix C, C =
  \alpha A A^T + \beta C when Trans is CblasNoTrans and C = \alpha A^T
  A + \beta C when Trans is CblasTrans. Since the matrix C is
  symmetric only its upper half or lower half need to be stored. When
  Uplo is CblasUpper then the upper triangle and diagonal of C are
  used, and when Uplo is CblasLower then the lower triangle and
  diagonal of C are used.")

(defmfun hermitian-rank-1-update
    ((x vector) (A matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "her")
  ((uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer x) :pointer) ((mpointer A) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (x A)
  :outputs (A)
  :documentation			; FDL
  "If the first argument is a vector,
  compute the hermitian rank-1 update A = alpha x x^H + A of the
  hermitian matrix A. Since the matrix A is hermitian only its upper
  half or lower half need to be stored. When Uplo is :upper then the
  upper triangle and diagonal of A are used, and when Uplo is
  :lower then the lower triangle and diagonal of A are used. The
  imaginary elements of the diagonal are automatically set to zero.
  If the first argument is a matrix, compute a rank-k update of the
  hermitian matrix C, C = \alpha A A^H + \beta C when Trans is
  :notrans and C = \alpha A^H A + \beta C when Trans is
  :trans. Since the matrix C is hermitian only its upper half or
  lower half need to be stored. When Uplo is :upper then the upper
  triangle and diagonal of C are used, and when Uplo is :lower
  then the lower triangle and diagonal of C are used. The imaginary
  elements of the diagonal are automatically set to zero.")

(defmfun symmetric-rank-2-update
    ((x vector) (y vector) (A matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "syr2")
  ((uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer x) :pointer)  ((mpointer y) :pointer)
   ((mpointer A) :pointer))
  :definition :generic
  :element-types :float
  :inputs (x y A)
  :outputs (A)
  :documentation			; FDL
  "If the first two arguments are vectors, compute
  the symmetric rank-2 update A = alpha x y^T + alpha y x^T + A of the
  symmetric matrix A. Since the matrix A is symmetric only its upper
  half or lower half need to be stored. When Uplo is :upper then the
  upper triangle and diagonal of A are used, and when Uplo is :lower
  then the lower triangle and diagonal of A are used.  If the first
  two arguments are matrices, compute a rank-2k update of the
  symmetric matrix C, C = \alpha A B^T + \alpha B A^T + \beta C when
  Trans is :notrans and C = \alpha A^T B + \alpha B^T A + \beta C
  when Trans is :trans. Since the matrix C is symmetric only its
  upper half or lower half need to be stored. When Uplo is :upper
  then the upper triangle and diagonal of C are used, and when Uplo is
  :lower then the lower triangle and diagonal of C are used.")

(defmfun hermitian-rank-2-update
    ((x vector) (y vector) (A matrix)
     &optional (alpha 1) (beta 1) (uplo :upper) (trans :notrans))
  ("gsl_blas_" :type "her2")
  ((uplo cblas-uplo) (alpha :element-c-type)
   ((mpointer x) :pointer) ((mpointer A) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (x A)
  :outputs (A)
  :documentation			; FDL
  "If the first two arguments are vectors, compute the
  hermitian rank-2 update A = alpha x y^H + alpha^* y x^H A of
  the hermitian matrix A. Since the matrix A is hermitian only its
  upper half or lower half need to be stored. When uplo is :upper
  then the upper triangle and diagonal of A are used, and when uplo is
  :lower then the lower triangle and diagonal of A are used. The
  imaginary elements of the diagonal are automatically set to zero.
  If the first two arguments are matrices, compute a rank-2k update of
  the hermitian matrix C, C = \alpha A B^H + \alpha^* B A^H + \beta C
  when Trans is :notrans and C = \alpha A^H B + \alpha^* B^H A +
  \beta C when Trans is :conjtrans. Since the matrix C is
  hermitian only its upper half or lower half need to be stored. When
  Uplo is :upper then the upper triangle and diagonal of C are
  used, and when Uplo is :lower then the lower triangle and
  diagonal of C are used. The imaginary elements of the diagonal are
  automatically set to zero.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(generate-all-array-tests matrix-product :float-complex
 (let ((m1 (array-default '(3 3)))
       (v1 (array-default 3))
       (v2 (array-default 3))
       (s1 (scalar-default))
       (s2 (scalar-default)))
   (cl-array (matrix-product m1 v1 v2 s1 s2))))

(generate-all-array-tests matrix-product-triangular :float-complex
 (let ((m1 (array-default '(3 3)))
       (v1 (array-default 3))
       (s1 (scalar-default)))
   (cl-array (matrix-product-triangular m1 v1 s1))))

(generate-all-array-tests inverse-matrix-product :float-complex
 (let ((m1 (array-default '(3 3)))
       (v1 (array-default 3))
       (s1 (scalar-default)))
   (cl-array (inverse-matrix-product m1 v1 s1))))

(generate-all-array-tests matrix-product-symmetric :float
 (let ((m1 (array-default '(3 3)))
	(v1 (array-default 3))
	(v3 (array-default 3))
	(s1 (scalar-default))
	(s2 (scalar-default)))
   (cl-array (matrix-product-symmetric m1 v1 v3 s1 s2))))

(generate-all-array-tests matrix-product-hermitian :complex
 (let ((m1 (array-default '(3 3)))
       (v1 (array-default 3))
       (v2 (array-default 3))
       (s1 (scalar-default))
       (s2 (scalar-default)))
   (cl-array (matrix-product-hermitian m1 v1 v2 s1 s2))))

#|
;;; Error, needs to be tracked down
;;; Matrix, vector lengths are not conformant invalid length in blas.c at line 1013
;;;   [Condition of type EBADLEN]

(generate-all-array-tests rank-1-update :float-complex
 (let ((m1 (array-default '(3 3)))
	(v1 (array-default 3))
	(v2 (array-default 3))
	(s1 (scalar-default)))
   (cl-array (rank-1-update s1 v1 v2 m1))))
|#
