;; QR decomposition
;; Liam Healy 2008-02-17 11:05:20EST qr.lisp
;; Time-stamp: <2008-02-17 11:17:37EST qr.lisp>
;; $Id$

(in-package :gsl)

;;; FDL
;;; A general rectangular M-by-N matrix A has a
;;; QR decomposition into the product of an orthogonal
;;; M-by-M square matrix Q (where Q^T Q = I) and
;;; an M-by-N right-triangular matrix R, A = Q R.

;;; This decomposition can be used to convert the linear system A x = b
;;; into the triangular system R x = Q^T b, which can be solved by
;;; back-substitution. Another use of the QR decomposition is to
;;; compute an orthonormal basis for a set of vectors. The first N
;;; columns of Q form an orthonormal basis for the range of A,
;;; ran(A), when A has full column rank.

(defmfun QR-decomp (A tau)
  "gsl_linalg_QR_decomp"
  (((pointer A) gsl-matrix-c) ((pointer tau) gsl-vector-c))
  :invalidate (A tau)
  :documentation 			; FDL
  "Factorize the M-by-N matrix A into the QR decomposition A = Q R.
   On output the diagonal and
   upper triangular part of the input matrix contain the matrix
   R.  The vector tau and the columns of the lower triangular
   part of the matrix A contain the Householder coefficients and
   Householder vectors which encode the orthogonal matrix Q.  The
   vector tau must be of length k=min(M,N). The matrix
   Q is related to these components by, Q = Q_k ... Q_2 Q_1
   where Q_i = I - \tau_i v_i v_i^T and v_i is the
   Householder vector v_i = (0,...,1,A(i+1,i),A(i+2,i),...,A(m,i)).
   This is the same storage scheme as used by lapack.

   The algorithm used to perform the decomposition is Householder QR (Golub
   & Van Loan, Matrix Computations, Algorithm 5.2.1).")

(defmfun QR-solve (QR tau b x)
    "gsl_linalg_QR_solve"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the square system A x = b using the QR
   decomposition of A into (QR, tau) given by
   QR-decomp. The least-squares solution for rectangular systems can
   be found using QR-lssolve.")

(defmfun QR-svx (QR tau x)
  "gsl_linalg_QR_svx"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solves the square system A x = b in-place using the
  QR decomposition of A into (QR, tau) given by
  QR-decomp.  On input x should contain the
  right-hand side b, which is replaced by the solution on output.")

(defmfun QR-lssolve (QR tau b x residual)
  "gsl_linalg_QR_lssolve"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c)
   ((pointer residual) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "The least squares solution to the overdetermined
   system A x = b where the matrix A has more rows than
   columns.  The least squares solution minimizes the Euclidean norm of the
   residual, ||Ax - b||.The routine uses the QR decomposition
   of A into (QR, tau) given by #'QR-decomp.  The solution is returned in x.
   The residual is computed as a by-product and stored in residual.")

(defmfun QR-QTvec (QR tau v)
  "gsl_linalg_QR_QTvec"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer v) gsl-vector-c))
  :invalidate (v)
  :documentation			; FDL
  "Apply the matrix Q^T encoded in the decomposition
   (QR, tau) to the vector v, storing the result Q^T v in v.
   The matrix multiplication is carried out directly using
   the encoding of the Householder vectors without needing to form the full
   matrix Q^T.")

(defmfun QR-Qvec (QR tau v)
  "gsl_linalg_QR_Qvec"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer v) gsl-vector-c))
  :invalidate (v)
  :documentation			; FDL
  "Apply the matrix Q encoded in the decomposition
   (QR, tau) to the vector v, storing the result Q v in v.
   The matrix multiplication is carried out directly using
   the encoding of the Householder vectors without needing to form the full
   matrix Q.")

(defmfun QR-Rsolve (QR b x)
  "gsl_linalg_QR_Rsolve"
  (((pointer QR) gsl-matrix-c) ((pointer b) gsl-vector-c)
   ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the triangular system R x = b for
   x.  It may be useful if the product b' = Q^T b has already
   been computed using QR-QTvec}.")

(defmfun QR-Rsvx (QR x)
  "gsl_linalg_QR_Rsvx"
  (((pointer QR) gsl-matrix-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the triangular system R x = b for x
  in-place. On input x should contain the right-hand side b
  and is replaced by the solution on output. This function may be useful if
  the product b' = Q^T b has already been computed using
  QR-QTvec}.")

(defmfun QR-unpack (QR tau Q R)
  "gsl_linalg_QR_unpack"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer Q) gsl-matrix-c) ((pointer R) gsl-matrix-c))
  :invalidate (Q R)
  :documentation			; FDL
  "Unpack the encoded QR decomposition
  (QR, tau) into the matrices Q and R where
  Q is M-by-M and R is M-by-N.")

(defmfun QR-QRsolve (Q R b x)
  "gsl_linalg_QR_QRsolve"
  (((pointer Q) gsl-matrix-c) ((pointer R) gsl-matrix-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solves the system R x = Q^T b for x.  It can
  be used when the QR decomposition of a matrix is available in
  unpacked form as Q, R).")

(defmfun QR-update (Q R w v)
  "gsl_linalg_QR_update"
  (((pointer Q) gsl-matrix-c) ((pointer R) gsl-matrix-c)
   ((pointer w) gsl-vector-c) ((pointer v) gsl-vector-c))
  :invalidate (w Q R)
  :return (Q R)
  :documentation			; FDL
  "Perform a rank-1 update w v^T of the QR
  decomposition (Q, R). The update is given by Q'R' = Q R + w v^T
  where the output matrices Q' and R' are also
  orthogonal and right triangular. Note that w is destroyed by the
  update.")

(defmfun R-solve (R b x)
  "gsl_linalg_R_solve"
  (((pointer R) gsl-matrix-c) ((pointer b) gsl-vector-c)
   ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solves the triangular system R x = b for the N-by-N matrix R.")

(defmfun R-svx (R x)
  "gsl_linalg_R_svx"
  (((pointer R) gsl-matrix-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the triangular system R x = b in-place. On
  input x should contain the right-hand side b, which is
  replaced by the solution on output.")
