;********************************************************
; file:        lu.lisp                                   
; description: LU decomposition                          
; date:        Thu Apr 27 2006 - 12:42                   
; author:      Liam Healy                                
; modified:    Thu Apr 27 2006 - 18:16
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Not ported: those functions that use complex vectors or matrices

(defun-gsl LU-decomp ((A gsl-matrix-c) (p gsl-permutation-c) (signum :int))
  "gsl_linalg_LU_decomp"
  :documentation "Factorize the square matrix @var{A} into the @math{LU}
  decomposition @math{PA = LU}.  On output the diagonal and upper
  triangular part of the input matrix @var{A} contain the matrix
  @math{U}. The lower triangular part of the input matrix (excluding the
  diagonal) contains @math{L}.  The diagonal elements of @math{L} are
  unity, and are not stored.

  The permutation matrix @math{P} is encoded in the permutation
  @var{p}. The @math{j}-th column of the matrix @math{P} is given by the
  @math{k}-th column of the identity matrix, where @math{k = p_j} the
  @math{j}-th element of the permutation vector. The sign of the
  permutation is given by @var{signum}. It has the value @math{(-1)^n},
  where @math{n} is the number of interchanges in the permutation.

  The algorithm used in the decomposition is Gaussian Elimination with
  partial pivoting (Golub & Van Loan, @cite{Matrix Computations},
  Algorithm 3.4.1).")

(defun-gsl LU-solve
    ((LU gsl-matrix-c) (p gsl-permutation-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_LU_solve"
  :documentation "Solve the square system @math{A x = b} using the @math{LU}
  decomposition of @math{A} into (@var{LU}, @var{p}) given by
  @code{gsl_linalg_LU_decomp} or @code{gsl_linalg_complex_LU_decomp}.")

(defun-gsl LU-svx ((LU gsl-matrix-c) (p gsl-permutation-c) (x gsl-vector-c))
  "gsl_linalg_LU_svx"
  :documentation "Solve the square system @math{A x = b} in-place
   using the @math{LU} decomposition of @math{A} into
   (@var{LU},@var{p}). On input @var{x} should contain the right-hand
   side @math{b}, which is replaced by the solution on output.")

(defun-gsl LU-refine
    ((A gsl-matrix-c) (LU gsl-matrix-c) (p gsl-permutation-c)
     (b gsl-vector-c) (x gsl-vector-c) (residual gsl-vector-c))
  "gsl_linalg_LU_refine"
  :documentation "Apply an iterative improvement to x, the solution of
  A x = b, using the LU decomposition of A into (LU,p). The initial
  residual r = A x - b is also computed and stored in residual. ")

(defun-gsl LU-invert
    ((LU gsl-matrix-c) (p gsl-permutation-c) (inverse gsl-matrix-c))
  "gsl_linalg_LU_invert"
  :documentation "Compute the inverse of a matrix A from its LU
   decomposition (LU,p), storing the result in the matrix inverse. The
   inverse is computed by solving the system A x = b for each column of
   the identity matrix. It is preferable to avoid direct use of the
   inverse whenever possible, as the linear solver functions can obtain
   the same result more efficiently and reliably (consult any
   introductory textbook on numerical linear algebra for details).")

(defun-gsl LU-det ((LU gsl-matrix-c) (signum :int))
  "gsl_linalg_LU_det"
  :documentation "Compute the determinant of a matrix A from its LU
  decomposition, LU. The determinant is computed as the product of the
  diagonal elements of U and the sign of the row permutation signum.")

(defun-gsl LU-lndet ((LU gsl-matrix-c))
  "gsl_linalg_LU_lndet"
  :documentation "The logarithm of the absolute value of the
   determinant of a matrix A, \ln|\det(A)|, from its LU decomposition,
   LU. This function may be useful if the direct computation of the
   determinant would overflow or underflow.")

(defun-gsl LU-sgndet ((LU gsl-matrix-c) (signum :int))
  "gsl_linalg_LU_sgndet"
  :documentation 
  "Compute the sign or phase factor of the determinant of a matrix A,
  \det(A)/|\det(A)|, from its LU decomposition, LU.")
