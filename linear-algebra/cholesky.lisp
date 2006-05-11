;********************************************************
; file:        cholesky.lisp                             
; description: Cholesky Decomposition
; date:        Wed May  3 2006 - 16:38                   
; author:      Liam Healy                                
; modified:    Wed May 10 2006 - 10:05
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; A symmetric, positive definite square matrix @math{A} has a Cholesky
;;; decomposition into a product of a lower triangular matrix @math{L} and
;;; its transpose @math{L^T},
;;; A = L L^T
;;; This is sometimes referred to as taking the square-root of a matrix. The
;;; Cholesky decomposition can only be carried out when all the eigenvalues
;;; of the matrix are positive.  This decomposition can be used to convert
;;; the linear system @math{A x = b} into a pair of triangular systems
;;; (@math{L y = b}, @math{L^T x = y}), which can be solved by forward and
;;; back-substitution.

(defun-gsl cholesky-decomp ((A gsl-matrix-c))
  "gsl_linalg_cholesky_decomp"
  :documentation
  "Factorize the positive-definite square matrix @var{A}
  into the Cholesky decomposition @math{A = L L^T}. On output the diagonal
  and lower triangular part of the input matrix @var{A} contain the matrix
  @math{L}. The upper triangular part of the input matrix contains
  @math{L^T}, the diagonal terms being identical for both @math{L} and
  @math{L^T}.  If the matrix is not positive-definite then the
  decomposition will fail, returning the error code :EDOM."
  :invalidate (A)
  :return-input (A))

(defun-gsl cholesky-solve
    ((cholesky gsl-matrix-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_cholesky_solve"
  :documentation "Solve the system @math{A x = b} using the Cholesky
  decomposition of @math{A} into the matrix @var{cholesky} given by
  #'cholesky-decomp."
  :invalidate (x)
  :return-input (x))

(defun-gsl cholesky-svx ((cholesky gsl-matrix-c) (x gsl-vector-c))
  "gsl_linalg_cholesky_svx"
  :documentation "Solve the system @math{A x = b} in-place using the
  Cholesky decomposition of @math{A} into the matrix @var{cholesky} given
  by @code{gsl_linalg_cholesky_decomp}. On input @var{x} should contain
  the right-hand side @math{b}, which is replaced by the solution on
  output."
  :invalidate (x)
  :return-input (x))
