;; Cholesky Decomposition
;; Liam Healy, Wed May  3 2006 - 16:38
;; Time-stamp: <2008-11-29 15:33:14EST cholesky.lisp>
;; $Id$

(in-package :gsl)

;;; FDL
;;; A symmetric, positive definite square matrix A has a Cholesky
;;; decomposition into a product of a lower triangular matrix L and
;;; its transpose L^T,
;;; A = L L^T
;;; This is sometimes referred to as taking the square-root of a matrix. The
;;; Cholesky decomposition can only be carried out when all the eigenvalues
;;; of the matrix are positive.  This decomposition can be used to convert
;;; the linear system A x = b into a pair of triangular systems
;;; (L y = b, L^T x = y), which can be solved by forward and
;;; back-substitution.

(defmfun cholesky-decomposition (A)
  "gsl_linalg_cholesky_decomp" (((mpointer A) :pointer))
  :inputs (A)
  :outputs (A)
  :return (A)
  :documentation			; FDL
  "Factorize the positive-definite square matrix A
  into the Cholesky decomposition A = L L^T. On output the diagonal
  and lower triangular part of the input matrix A contain the matrix
  L.  The upper triangular part of the input matrix contains
  L^T, the diagonal terms being identical for both L and
  L^T.  If the matrix is not positive-definite then the
  decomposition will fail, returning the error input-domain.")

(defmfun cholesky-solve (A x b)
  "gsl_linalg_cholesky_solve"
  (((mpointer A) :pointer) ((mpointer b) :pointer)
   ((mpointer x) :pointer))
  :inputs (A b)
  :outputs (x)
  :return (x)
  :documentation			; FDL
  "Solve the system A x = b using the Cholesky
  decomposition of A into the matrix cholesky given by
  #'cholesky-decomposition.")

(defmfun cholesky-solvex (A x)
  "gsl_linalg_cholesky_svx"
  (((mpointer A) :pointer) ((mpointer x) :pointer))
  :inputs (A x)
  :outputs (x)
  :return (x)
  :documentation			; FDL
  "Solve the system A x = b in-place using the
  Cholesky decomposition of A into the matrix cholesky given
  by #'cholesky-decomposition. On input x should contain
  the right-hand side B, which is replaced by the solution on
  output.")
