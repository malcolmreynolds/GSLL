;********************************************************
; file:        qrpt.lisp                                   
; description: QR with column pivoting
; date:        Fri Apr 28 2006 - 16:53                   
; author:      Liam Healy                                
; modified:    Mon Jul  3 2006 - 23:21
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl QRPT-decomp (A tau p signum norm)
  "gsl_linalg_QRPT_decomp"
  (((pointer A) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer p) gsl-permutation-c)
   (signum :pointer) ((pointer norm) gsl-vector-c))
  :documentation "Factorizes the @math{M}-by-@math{N} matrix @var{A} into
   the @math{QRP^T} decomposition @math{A = Q R P^T}.  On output the
   diagonal and upper triangular part of the input matrix contain the
   matrix @math{R}. The permutation matrix @math{P} is stored in the
   permutation @var{p}.  The sign of the permutation is given by
   @var{signum}. It has the value @math{(-1)^n}, where @math{n} is the
   number of interchanges in the permutation. The vector @var{tau} and the
   columns of the lower triangular part of the matrix @var{A} contain the
   Householder coefficients and vectors which encode the orthogonal matrix
   @var{Q}.  The vector @var{tau} must be of length @math{k=\min(M,N)}. The
   matrix @math{Q} is related to these components by, @math{Q = Q_k ... Q_2
   Q_1} where @math{Q_i = I - \tau_i v_i v_i^T} and @math{v_i} is the
   Householder vector @math{v_i =
   (0,...,1,A(i+1,i),A(i+2,i),...,A(m,i))}. This is the same storage scheme
   as used by @sc{lapack}.  The vector @var{norm} is a workspace of length
   @var{N} used for column pivoting.

   The algorithm used to perform the decomposition is Householder QR with
   column pivoting (Golub & Van Loan, @cite{Matrix Computations}, Algorithm
   5.4.1)."
  :invalidate (A tau p norm)
  :return (A tau signum p))

(defun-gsl QRPT-decomp2 (A q r tau p signum norm)
  "gsl_linalg_QRPT_decomp2"
  (((pointer A) gsl-matrix-c) ((pointer q) gsl-matrix-c)
   ((pointer r) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer p) gsl-permutation-c) (signum :pointer)
   ((pointer norm) gsl-vector-c))
  :documentation "Factorize the matrix @var{A} into the decomposition
  @math{A = Q R P^T} without modifying @var{A} itself and storing the
  output in the separate matrices @var{q} and @var{r}."
  :invalidate (q r norm)
  :return (q r p signum))

(defun-gsl QRPT-solve (QR tau p b x)
  "gsl_linalg_QRPT_solve"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer p) gsl-permutation-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :documentation "Solve the square system @math{A x = b} using the @math{QRP^T}
   decomposition of @math{A} into (@var{QR}, @var{tau}, @var{p}) given by
   #'QRPT-decomp."
  :invalidate (x)
  :return (x))

(defun-gsl QRPT-svx (QR tau p x)
  "gsl_linalg_QRPT_svx"
  (((pointer QR) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer p) gsl-permutation-c) ((pointer x) gsl-vector-c))
  :documentation "Solve the square system @math{A x = b} in-place using the
  @math{QRP^T} decomposition of @math{A} into
  (@var{QR},@var{tau},@var{p}). On input @var{x} should contain the
   right-hand side @math{b}, which is replaced by the solution on output."
  :invalidate (x)
  :return (x))

(defun-gsl QRPT-QRsolve (QR p b x)
  "gsl_linalg_QRPT_QRsolve"
  (((pointer QR) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :documentation "Solve the square system @math{R P^T x = Q^T b} for
  @var{x}. It can be used when the @math{QR} decomposition of a matrix is
  available in unpacked form as (@var{Q}, @var{R})."
  :invalidate (x)
  :return (x))

(defun-gsl QRPT-update (Q R p w v)
  "gsl_linalg_QRPT_update"
  (((pointer Q) gsl-matrix-c) ((pointer R) gsl-matrix-c)
   ((pointer p) gsl-permutation-c)
   ((pointer w) gsl-vector-c) ((pointer v) gsl-vector-c))
  :documentation "Perform a rank-1 update @math{w v^T} of the @math{QRP^T}
  decomposition (@var{Q}, @var{R}, @var{p}). The update is given by
  @math{Q'R' = Q R + w v^T} where the output matrices @math{Q'} and
  @math{R'} are also orthogonal and right triangular. Note that @var{w} is
  destroyed by the update. The permutation @var{p} is not changed."
  :invalidate (w Q R)
  :return (Q R))

(defun-gsl QRPT-Rsolve (QR p b x)
  "gsl_linalg_QRPT_Rsolve"
  (((pointer QR) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :documentation "Solve the triangular system @math{R P^T x = b} for the
   @math{N}-by-@math{N} matrix @math{R} contained in @var{QR}."
  :invalidate (x)
  :return (x))

(defun-gsl QRPT-Rsvx (QR p x)
  "gsl_linalg_QRPT_Rsvx"
  (((pointer QR) gsl-matrix-c) ((pointer p) gsl-permutation-c)
   ((pointer x) gsl-vector-c))
  :documentation "Solve the triangular system @math{R P^T x = b} in-place
  for the @math{N}-by-@math{N} matrix @math{R} contained in @var{QR}. On
  input @var{x} should contain the right-hand side @math{b}, which is
  replaced by the solution on output."
  :invalidate (x)
  :return (x))
