;; Singular Value Decomposition
;; Liam Healy, Tue May  2 2006 - 12:15
;; Time-stamp: <2008-02-17 11:29:50EST svd.lisp>
;; $Id$

(in-package :gsl)

;;; FDL
;;; A general rectangular M-by-N matrix A has a
;;; singular value decomposition (svd) into the product of an
;;; M-by-N orthogonal matrix U, an N-by- diagonal matrix of
;;; singular values S and the transpose of an
;;; N-by-N orthogonal square matrix V, A = U S V^T
;;; The singular values sigma_i = S_{ii} are all non-negative and are
;;; generally chosen to form a non-increasing sequence 
;;; sigma_1 >= sigma_2 >= ... >= sigma_N >= 0.

;;; The singular value decomposition of a matrix has many practical uses.
;;; The condition number of the matrix is given by the ratio of the largest
;;; singular value to the smallest singular value. The presence of a zero
;;; singular value indicates that the matrix is singular. The number of
;;; non-zero singular values indicates the rank of the matrix.  In practice
;;; singular value decomposition of a rank-deficient matrix will not produce
;;; exact zeroes for singular values, due to finite numerical
;;; precision. Small singular values should be edited by choosing a suitable
;;; tolerance.

(defmfun SV-decomp (A V S work)
  "gsl_linalg_SV_decomp"
  (((pointer A) gsl-matrix-c) ((pointer V) gsl-matrix-c)
   ((pointer S) gsl-vector-c) ((pointer work) gsl-vector-c))
  :invalidate (A)
  :documentation			; FDL
  "Factorize the M-by-N matrix A into
  the singular value decomposition A = U S V^T for M >= N.
  On output the matrix A is replaced by U.  The diagonal elements
  of the singular value matrix S
  are stored in the vector S.  The singular values are non-negative
  and form a non-increasing sequence from S_1 to S_N. The
  matrix V contains the elements of V in untransposed
  form. To form the product U S V^T it is necessary to take the
  transpose of V.  A workspace of length N is required in work.
  This routine uses the Golub-Reinsch SVD algorithm.")

(defmfun SV-decomp-mod (A X V S work)
  "gsl_linalg_SV_decomp_mod"
  (((pointer A) gsl-matrix-c) ((pointer X) gsl-matrix-c)
   ((pointer V) gsl-matrix-c)
   ((pointer S) gsl-vector-c) ((pointer work) gsl-vector-c))
  :invalidate (A)
  :documentation			; FDL
  "The SVD using the modified Golub-Reinsch algorithm, which is
   faster for M >> N.  It requires the vector work of length N and the
   N-by-N matrix X as additional working space.")

(defmfun SV-decomp-jacobi (A V S)
  "gsl_linalg_SV_decomp_jacobi"
  (((pointer A) gsl-matrix-c) ((pointer V) gsl-matrix-c)
   ((pointer S) gsl-vector-c))
  :invalidate (A)
  :documentation			; FDL
  "The SVD of the M-by-N matrix A using one-sided Jacobi
   orthogonalization for M >= N.  The Jacobi method can compute singular
   values to higher relative accuracy than Golub-Reinsch algorithms (see
   references for details).")

(defmfun SV-solve (U V S b x)
  "gsl_linalg_SV_solve"
  (((pointer U) gsl-matrix-c) ((pointer V) gsl-matrix-c)
   ((pointer S) gsl-vector-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the system A x = b using the singular value
   decomposition (U, S, V) of A given by #'SV-decomp.

   Only non-zero singular values are used in computing the solution. The
   parts of the solution corresponding to singular values of zero are
   ignored.  Other singular values can be edited out by setting them to
   zero before calling this function. 

   In the over-determined case where A has more rows than columns the
   system is solved in the least squares sense, returning the solution
   x which minimizes ||A x - b||_2.")
