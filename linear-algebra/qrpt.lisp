;; QR with column pivoting
;; Liam Healy, Fri Apr 28 2006 - 16:53
;; Time-stamp: <2009-02-23 22:11:10EST qrpt.lisp>
;; $Id$

(in-package :gsl)

(defmfun QRPT-decomposition
    (A tau p &optional (norm (make-marray 'double-float :dimensions (dim1 A))))
  "gsl_linalg_QRPT_decomp"
  (((mpointer A) :pointer) ((mpointer tau) :pointer)
   ((mpointer p) :pointer)
   (signum :int) ((mpointer norm) :pointer))
  :inputs (A)
  :outputs (A tau p norm)
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

(defmfun QRPT-decomposition*
    (A q r tau p &optional (norm (make-marray 'double-float :dimensions (dim1 A))))
  "gsl_linalg_QRPT_decomp2"
  (((mpointer A) :pointer) ((mpointer q) :pointer)
   ((mpointer r) :pointer) ((mpointer tau) :pointer)
   ((mpointer p) :pointer) (signum :int)
   ((mpointer norm) :pointer))
  :inputs (A)
  :outputs (q r norm)
  :return (q r p signum)
  :documentation			; FDL
  "Factorize the matrix A into the decomposition
  A = Q R P^T without modifying A itself and storing the
  output in the separate matrices q and r.")

(defmfun QRPT-solve
    (QR tau p b &optional x-spec
       &aux
       (x (if (eq x-spec t)
	      (make-marray 'double-float :dimensions (dimensions b))
	      x-spec)))
  ("gsl_linalg_QRPT_svx" "gsl_linalg_QRPT_solve")
  ((((mpointer QR) :pointer) ((mpointer tau) :pointer) ((mpointer p) :pointer)
    ((mpointer b) :pointer))
   (((mpointer QR) :pointer) ((mpointer tau) :pointer) ((mpointer p) :pointer)
    ((mpointer b) :pointer) ((mpointer x) :pointer)))
  :inputs (QR tau p b x)
  :outputs (x)
  :return ((or x b))
  :documentation			; FDL
  "Solve the square system A x = b using the QRP^T decomposition of A
   into (QR, tau, p) given by #'QRPT-decomposition.  If x-spec is
   NIL (default), the solution will replace b.  If x-spec is T, then
   an array will be created and the solution returned in it.  If
   x-spec is a marray, the solution will be returned in it.  If x-spec
   is non-NIL, on output the solution is stored in x and b is not
   modified.  The solution is returned from the function call.")

(defmfun QRPT-QRsolve
    (QR p b &optional (x (make-marray 'double-float :dimensions (dimensions b))))
  "gsl_linalg_QRPT_QRsolve"
  (((mpointer QR) :pointer) ((mpointer p) :pointer)
   ((mpointer b) :pointer) ((mpointer x) :pointer))
  :inputs (QR p b)
  :outputs (x)
  :documentation			; FDL
  "Solve the square system R P^T x = Q^T b for
   x. It can be used when the QR decomposition of a matrix is
   available in unpacked form as (Q, R).")

(defmfun QRPT-update (Q R p w v)
  "gsl_linalg_QRPT_update"
  (((mpointer Q) :pointer) ((mpointer R) :pointer)
   ((mpointer p) :pointer)
   ((mpointer w) :pointer) ((mpointer v) :pointer))
  :inputs (Q R p w v)
  :outputs (w Q R)
  :return (Q R)
  :documentation			; FDL
  "Perform a rank-1 update w v^T of the QRP^T
   decomposition (Q, R, p). The update is given by
   Q'R' = Q R + w v^T where the output matrices Q' and
   R' are also orthogonal and right triangular. Note that w is
   destroyed by the update. The permutation p is not changed.")

(defmfun QRPT-Rsolve
    (QR p b &optional x-spec
       &aux
       (x (if (eq x-spec t)
	      (make-marray 'double-float :dimensions (dimensions b))
	      x-spec)))
  ("gsl_linalg_QRPT_Rsvx" "gsl_linalg_QRPT_Rsolve")
  ((((mpointer QR) :pointer) ((mpointer p) :pointer) ((mpointer b) :pointer))
   (((mpointer QR) :pointer) ((mpointer p) :pointer)
    ((mpointer b) :pointer) ((mpointer x) :pointer)))
  :inputs (QR p b x)
  :outputs (x b)
  :return ((or x b))
  :documentation			; FDL
  "Solve the triangular system R P^T x = b in-place for the N-by-N
  matrix R contained in QR. On input x should contain the right-hand
  side b, which is replaced by the solution on output.  If x-spec is
  NIL (default), the solution will replace b.  If x-spec is T, then an
  array will be created and the solution returned in it.  If x-spec is
  a marray, the solution will be returned in it.  If x-spec is
  non-NIL, on output the solution is stored in x and b is not
  modified.  The solution is returned from the function call.")
