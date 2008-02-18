;; QR with column pivoting
;; Liam Healy, Fri Apr 28 2006 - 16:53
;; Time-stamp: <2008-02-17 11:24:34EST qrpt.lisp>
;; $Id: $

(in-package :gsl)

(defmfun QRPT-decomp (A tau p signum norm)
  "gsl_linalg_QRPT_decomp"
  (((pointer A) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer p) gsl-permutation-c)
   (signum :pointer) ((pointer norm) gsl-vector-c))
  :invalidate (A tau p norm)
  :return (A tau signum p)
  :documentation			; FDL
  "Factorizes the M-by-N matrix A into
   the QRP^T decomposition A = Q R P^T.  On output the
   diagonal and upper triangular part of the input matrix contain the
   matrix R.  The permutation matrix P is stored in the
   permutation p.  The sign of the permutation is given by
   signum. It has the value (-1)^n, where n is the
   number of interchanges in the permutation. The vector tau and the
   columns of the lower triangular part of the matrix A contain the
   Householder coefficients and vectors which encode the orthogonal matrix
   Q.  The vector tau must be of length k=min(M,N). The
   matrix Q is related to these components by, Q = Q_k ... Q_2 Q_1
   where Q_i = I - tau_i v_i v_i^T and v_i is the Householder vector
   v_i = (0,...,1,A(i+1,i),A(i+2,i),...,A(m,i)). This is the same storage scheme
   as used by lapack.  The vector norm is a workspace of length
   N used for column pivoting.

   The algorithm used to perform the decomposition is Householder QR with
   column pivoting (Golub & Van Loan, Matrix Computations, Algorithm
   5.4.1).")

(defmfun QRPT-decomp2 (A q r tau p signum norm)
  "gsl_linalg_QRPT_decomp2"
  (((pointer A) gsl-matrix-c) ((pointer q) gsl-matrix-c)
   ((pointer r) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer p) gsl-permutation-c) (signum :pointer)
   ((pointer norm) gsl-vector-c))
  :invalidate (q r norm)
  :return (q r p signum)
  :documentation			; FDL
  "Factorize the matrix A into the decomposition
  A = Q R P^T without modifying A itself and storing the
  output in the separate matrices q and r.")

(defmfun QRPT-solve (QR tau p b x)
  "gsl_linalg_QRPT_solve"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer p) gsl-permutation-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :return (x)
  :documentation 			; FDL
  "Solve the square system A x = b using the QRP^T
   decomposition of A into (QR, tau, p) given by #'QRPT-decomp.")

(defmfun QRPT-svx (QR tau p x)
  "gsl_linalg_QRPT_svx"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer p) gsl-permutation-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :return (x)
  :documentation			; FDL
  "Solve the square system A x = b in-place using the
   QRP^T decomposition of A into (QR, tau, p). On input x should contain the
   right-hand side b, which is replaced by the solution on output.")

(defmfun QRPT-QRsolve (QR p b x)
  "gsl_linalg_QRPT_QRsolve"
  (((pointer QR) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :return (x)
  :documentation			; FDL
  "Solve the square system R P^T x = Q^T b for
   x. It can be used when the QR decomposition of a matrix is
   available in unpacked form as (Q, R).")

(defmfun QRPT-update (Q R p w v)
  "gsl_linalg_QRPT_update"
  (((pointer Q) gsl-matrix-c) ((pointer R) gsl-matrix-c)
   ((pointer p) gsl-permutation-c)
   ((pointer w) gsl-vector-c) ((pointer v) gsl-vector-c))
  :invalidate (w Q R)
  :return (Q R)
  :documentation			; FDL
  "Perform a rank-1 update w v^T of the QRP^T
   decomposition (Q, R, p). The update is given by
   Q'R' = Q R + w v^T where the output matrices Q' and
   R' are also orthogonal and right triangular. Note that w is
   destroyed by the update. The permutation p is not changed.")

(defmfun QRPT-Rsolve (QR p b x)
  "gsl_linalg_QRPT_Rsolve"
  (((pointer QR) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :return (x)
  :documentation			; FDL
  "Solve the triangular system R P^T x = b for the
   N-by-N matrix R contained in QR.")

(defmfun QRPT-Rsvx (QR p x)
  "gsl_linalg_QRPT_Rsvx"
  (((pointer QR) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer x) gsl-vector-c))
  :invalidate (x)
  :return (x)
  :documentation			; FDL
  "Solve the triangular system R P^T x = b in-place
  for the N-by-N matrix R contained in QR. On
  input x should contain the right-hand side b, which is
  replaced by the solution on output.")
