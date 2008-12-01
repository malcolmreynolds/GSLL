;; LU decomposition
;; Liam Healy, Thu Apr 27 2006 - 12:42
;; Time-stamp: <2008-11-30 23:44:35EST lu.lisp>
;; $Id$

(in-package :gsl)

(defmfun LU-decomposition ((A matrix) p)
  ("gsl_linalg" :complex "_LU_decomp")
  (((mpointer A) :pointer) ((mpointer p) :pointer) (signum :int))
  :definition :generic
  :inputs (A)
  :outputs (A p)
  :return (A)
  :element-types :doubles
  :documentation			; FDL
  "Factorize the square matrix A into the LU decomposition PA = LU,
  and return the sign of the permutation.  On output the diagonal and
  upper triangular part of the input matrix A contain the matrix U.
  The lower triangular part of the input matrix (excluding the
  diagonal) contains L.  The diagonal elements of L are unity, and are
  not stored.

  The permutation matrix P is encoded in the permutation
  p.  The j-th column of the matrix P is given by the
  k-th column of the identity matrix, where k = p_j the
  j-th element of the permutation vector. The sign of the
  permutation is given by signum. It has the value (-1)^n,
  where n is the number of interchanges in the permutation.

  The algorithm used in the decomposition is Gaussian Elimination with
  partial pivoting (Golub & Van Loan, Matrix Computations,
  Algorithm 3.4.1).")

(defmfun LU-solve ((LU matrix) p (b vector) (x vector))
  ("gsl_linalg" :complex "_LU_solve")
  (((mpointer LU) :pointer) ((mpointer p) :pointer)
   ((mpointer b) :pointer) ((mpointer x) :pointer))
  :definition :generic
  :inputs (LU p b)
  :outputs (x)
  :return (x)
  :element-types :doubles
  :documentation			; FDL
  "Solve the square system A x = b using the LU
  decomposition of A into (LU, p) given by LU-decomp.")

(defmfun LU-solvex ((LU matrix) p (x vector))
  ("gsl_linalg" :complex "_LU_svx")
  (((mpointer LU) :pointer) ((mpointer p) :pointer)
   ((mpointer x) :pointer))
  :definition :generic
  :inputs (LU p)
  :outputs (x)
  :return (x)
  :element-types :doubles
  :documentation			; FLD
  "Solve the square system A x = b in-place
   using the LU decomposition of A into
   (LU, p). On input x should contain the right-hand
   side b, which is replaced by the solution on output.")

(defmfun LU-refine ((A matrix) LU p (b vector) (x vector) residual)
  ("gsl_linalg" :complex "_LU_refine")
  (((mpointer A) :pointer) ((mpointer LU) :pointer)
   ((mpointer p) :pointer)
   ((mpointer b) :pointer) ((mpointer x) :pointer)
   ((mpointer residual) :pointer))
  :definition :generic
  :inputs (LU p)
  :outputs (x residual)
  :return (x)
  :element-types :doubles
  :documentation			; FDL
  "Apply an iterative improvement to x, the solution of
  A x = b, using the LU decomposition of A into (LU,p). The initial
  residual r = A x - b is also computed and stored in residual. ")

(defmfun LU-invert ((LU matrix) p inverse)
  ("gsl_linalg" :complex "_LU_invert")
  (((mpointer LU) :pointer) ((mpointer p) :pointer)
   ((mpointer inverse) :pointer))
  :definition :generic
  :inputs (LU p)
  :outputs (inverse)
  :return (inverse)
  :element-types :doubles
  :documentation			; FDL
  "Compute the inverse of a matrix A from its LU
   decomposition (LU,p), storing the result in the matrix inverse. The
   inverse is computed by solving the system A x = b for each column of
   the identity matrix. It is preferable to avoid direct use of the
   inverse whenever possible, as the linear solver functions can obtain
   the same result more efficiently and reliably (consult any
   introductory textbook on numerical linear algebra for details).")

(defmfun LU-determinant ((LU matrix) signum)
  ("gsl_linalg" :complex "_LU_det")
  (((mpointer LU) :pointer) (signum :int))
  :c-return :double
  :definition :generic
  :inputs (LU)
  :element-types :doubles
  :documentation			; FDL
  "Compute the determinant of a matrix from its LU
  decomposition, LU. The determinant is computed as the product of the
  diagonal elements of U and the sign of the row permutation signum.")


(defmfun LU-log-determinant ((LU matrix))
  ("gsl_linalg" :complex "_LU_lndet")
  (((mpointer LU) :pointer))
  :c-return :double
  :definition :generic
  :inputs (LU)
  :element-types :doubles
  :documentation			; FDL
  "The logarithm of the absolute value of the
   determinant of a matrix A, ln|det(A)|, from its LU decomposition,
   LU. This function may be useful if the direct computation of the
   determinant would overflow or underflow.")

(defmfun LU-sgndet ((LU matrix) signum)
  ("gsl_linalg" :complex "_LU_sgndet")
  (((mpointer LU) :pointer) (signum :int))
  :c-return :int
  :definition :generic
  :inputs (LU)
  :element-types :doubles
  :documentation 			; FDL
  "Compute the sign or phase factor of the determinant of a matrix A,
  det(A)/|det(A)|, from its LU decomposition, LU.")

;;; Examples and unit test

(export 'invert-matrix)
(defun invert-matrix (mat)
  "Invert the matrix."
  (letm ((mmat mat)
	 (dim (array-dimension mat 0))
	 (per (permutation dim))
	 (inv (make-array* 'double-float :dimensions (list dim dim))))
    (LU-decomposition mmat per)
    (lu-invert mmat per inv)
    (cl-array inv)))

(save-test lu
 (invert-matrix
  (make-array* '(2 2) 'double-float :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0))))
