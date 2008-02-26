;; LU decomposition
;; Liam Healy, Thu Apr 27 2006 - 12:42
;; Time-stamp: <2008-02-17 11:05:06EST lu.lisp>
;; $Id$

(in-package :gsl)

;;; Not ported: those functions that use complex vectors or matrices

(defmfun LU-decomp (A p signum)
  "gsl_linalg_LU_decomp"
  (((pointer A) gsl-matrix-c) ((pointer p) gsl-permutation-c) (signum :int))
  :invalidate (A p)
  :documentation			; FDL
  "Factorize the square matrix A into the LU
  decomposition PA = LU.  On output the diagonal and upper
  triangular part of the input matrix A contain the matrix
  U.  The lower triangular part of the input matrix (excluding the
  diagonal) contains L.  The diagonal elements of L are
  unity, and are not stored.

  The permutation matrix P is encoded in the permutation
  p.  The j-th column of the matrix P is given by the
  k-th column of the identity matrix, where k = p_j the
  j-th element of the permutation vector. The sign of the
  permutation is given by signum. It has the value (-1)^n,
  where n is the number of interchanges in the permutation.

  The algorithm used in the decomposition is Gaussian Elimination with
  partial pivoting (Golub & Van Loan, Matrix Computations,
  Algorithm 3.4.1).")

(defmfun LU-solve (LU p b x)
  "gsl_linalg_LU_solve"
  (((pointer LU) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the square system A x = b using the LU
  decomposition of A into (LU, p) given by LU-decomp.")

(defmfun LU-svx (LU p x)
  "gsl_linalg_LU_svx"
  (((pointer LU) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FLD
  "Solve the square system A x = b in-place
   using the LU decomposition of A into
   (LU, p). On input x should contain the right-hand
   side b, which is replaced by the solution on output.")

(defmfun LU-refine (A LU p b x residual)
  "gsl_linalg_LU_refine"
  (((pointer A) gsl-matrix-c) ((pointer LU) gsl-matrix-c)
   ((pointer p) gsl-permutation-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c)
   ((pointer residual) gsl-vector-c))
  :invalidate (x residual)
  :documentation			; FDL
  "Apply an iterative improvement to x, the solution of
  A x = b, using the LU decomposition of A into (LU,p). The initial
  residual r = A x - b is also computed and stored in residual. ")

(defmfun LU-invert (LU p inverse)
  "gsl_linalg_LU_invert"
  (((pointer LU) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer inverse) gsl-matrix-c))
  :invalidate (inverse)
  :documentation			; FDL
  "Compute the inverse of a matrix A from its LU
   decomposition (LU,p), storing the result in the matrix inverse. The
   inverse is computed by solving the system A x = b for each column of
   the identity matrix. It is preferable to avoid direct use of the
   inverse whenever possible, as the linear solver functions can obtain
   the same result more efficiently and reliably (consult any
   introductory textbook on numerical linear algebra for details).")

(defgeneric LU-det (LU signum)
  (:documentation			; FDL
   "Compute the determinant of a matrix A from its LU
  decomposition, LU. The determinant is computed as the product of the
  diagonal elements of U and the sign of the row permutation signum."))

(defmfun LU-det ((LU gsl-matrix-double) signum)
  "gsl_linalg_LU_det"
  (((pointer LU) gsl-matrix-c) (signum :int))
  :type :method
  :c-return :double)

(defmfun LU-det ((LU gsl-matrix-complex) signum)
  "gsl_linalg_complex_LU_det"
  (((pointer LU) gsl-matrix-c) (signum :int))
  :type :method
  :c-return (ret gsl-complex)
  :return ((complex-to-cl ret)))

(defmfun LU-lndet (LU)
  "gsl_linalg_LU_lndet"
  (((pointer LU) gsl-matrix-c))
  :c-return :double
  :documentation			; FDL
  "The logarithm of the absolute value of the
   determinant of a matrix A, ln|det(A)|, from its LU decomposition,
   LU. This function may be useful if the direct computation of the
   determinant would overflow or underflow.")

(defmfun LU-sgndet (LU signum)
  "gsl_linalg_LU_sgndet"
  (((pointer LU) gsl-matrix-c) (signum :int))
  :c-return :int
  :documentation 			; FDL
  "Compute the sign or phase factor of the determinant of a matrix A,
  det(A)/|det(A)|, from its LU decomposition, LU.")
