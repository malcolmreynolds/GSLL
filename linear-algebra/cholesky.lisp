;; Cholesky Decomposition
;; Liam Healy, Wed May  3 2006 - 16:38
;; Time-stamp: <2009-02-16 23:18:32EST cholesky.lisp>
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

;;; GSL version 1.10 introduced functions for complex matrices.

(defmfun cholesky-decomposition ((A matrix))
  ("gsl_linalg" :complex "_cholesky_decomp")
  (((mpointer A) :pointer))
  :definition :generic
  :element-types :doubles
  :inputs (A)
  :outputs (A)
  :documentation			; FDL
  "Factorize the positive-definite square matrix A into the Cholesky
  decomposition A = L L^T (real) or A = L L^H (complex).  On output the
  diagonal and lower triangular part of the input matrix A contain the
  matrix L.  The upper triangular part of the input matrix contains
  L^T, the diagonal terms being identical for both L and L^T.  If the
  matrix is not positive-definite then the decomposition will fail,
  returning the error input-domain.")

(defmfun cholesky-solve
    ((A matrix) (b vector) &optional x)
  (("gsl_linalg" :complex "_cholesky_svx")
   ("gsl_linalg" :complex "_cholesky_solve"))
  ((((mpointer A) :pointer) ((mpointer b) :pointer))
   (((mpointer A) :pointer) ((mpointer b) :pointer)
    ((mpointer x) :pointer)))
  :definition :generic
  :element-types :doubles
  :inputs (A b)
  :outputs (x)
  :documentation			; FDL
  "Solve the system A x = b using the Cholesky
  decomposition of A into the matrix given by
  #'cholesky-decomposition.  If x is not specified, the solution
  will replace b.")
