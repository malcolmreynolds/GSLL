;********************************************************
; file:        qr.lisp                                   
; description: QR decomposition                          
; date:        Fri Apr 28 2006 - 16:53                   
; author:      Liam Healy                                
; modified:    Fri Apr 28 2006 - 21:03
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; A general rectangular @math{M}-by-@math{N} matrix @math{A} has a
;;; @math{QR} decomposition into the product of an orthogonal
;;; @math{M}-by-@math{M} square matrix @math{Q} (where @math{Q^T Q = I}) and
;;; an @math{M}-by-@math{N} right-triangular matrix @math{R},
;;; A = Q R

;;; This decomposition can be used to convert the linear system @math{A x =
;;; b} into the triangular system @math{R x = Q^T b}, which can be solved by
;;; back-substitution. Another use of the @math{QR} decomposition is to
;;; compute an orthonormal basis for a set of vectors. The first @math{N}
;;; columns of @math{Q} form an orthonormal basis for the range of @math{A},
;;; @math{ran(A)}, when @math{A} has full column rank.

(defun-gsl QR-decomp ((A gsl-matrix-c) (tau gsl-vector-c))
  "gsl_linalg_QR_decomp"
  :documentation "Factorize the @math{M}-by-@math{N} matrix @var{A} into
   the @math{QR} decomposition @math{A = Q R}.  On output the diagonal and
   upper triangular part of the input matrix contain the matrix
   @math{R}. The vector @var{tau} and the columns of the lower triangular
   part of the matrix @var{A} contain the Householder coefficients and
   Householder vectors which encode the orthogonal matrix @var{Q}.  The
   vector @var{tau} must be of length @math{k=\min(M,N)}. The matrix
   @math{Q} is related to these components by, @math{Q = Q_k ... Q_2 Q_1}
   where @math{Q_i = I - \tau_i v_i v_i^T} and @math{v_i} is the
   Householder vector @math{v_i =
   (0,...,1,A(i+1,i),A(i+2,i),...,A(m,i))}. This is the same storage scheme
   as used by @sc{lapack}.

   The algorithm used to perform the decomposition is Householder QR (Golub
   & Van Loan, @cite{Matrix Computations}, Algorithm 5.2.1)."
  :after ((cl-invalidate A))
  :return-input (A tau))

(defun-gsl QR-solve
    ((QR gsl-matrix-c) (tau gsl-vector-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_QR_solve"
  :documentation "Solve the square system @math{A x = b} using the @math{QR}
   decomposition of @math{A} into (@var{QR}, @var{tau}) given by
   QR-decomp. The least-squares solution for rectangular systems can
   be found using QR-lssolve."
  :after ((cl-invalidate x))
  :return-input (x))

(defun-gsl QR-svx ((QR gsl-matrix-c) (tau gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_QR_svx"
  :documentation "Solves the square system @math{A x = b} in-place using the
  @math{QR} decomposition of @math{A} into (@var{QR},@var{tau}) given by
  QR-decomp.  On input @var{x} should contain the
  right-hand side @math{b}, which is replaced by the solution on output."
  :after ((cl-invalidate x))
  :return-input (x))

(defun-gsl QR-lssolve
    ((QR gsl-matrix-c) (tau gsl-vector-c) (b gsl-vector-c) (x gsl-vector-c)
     (residual gsl-vector-c))
  "gsl_linalg_QR_lssolve"
  :documentation "The least squares solution to the overdetermined
   system @math{A x = b} where the matrix @var{A} has more rows than
   columns.  The least squares solution minimizes the Euclidean norm of the
   residual, @math{||Ax - b||}.The routine uses the @math{QR} decomposition
   of @math{A} into (@var{QR}, @var{tau}) given by
   @code{gsl_linalg_QR_decomp}.  The solution is returned in @var{x}.  The
   residual is computed as a by-product and stored in @var{residual}."
  :after ((cl-invalidate x))
  :return-input (x))

(defun-gsl QR-QTvec ((QR gsl-matrix-c) (tau gsl-vector-c) (v gsl-vector-c))
  "gsl_linalg_QR_QTvec"
  :documentation "Apply the matrix @math{Q^T} encoded in the decomposition
  (@var{QR},@var{tau}) to the vector @var{v}, storing the result @math{Q^T
  v} in @var{v}.  The matrix multiplication is carried out directly using
  the encoding of the Householder vectors without needing to form the full
  matrix @math{Q^T}."
  :after ((cl-invalidate v))
  :return-input (v))

(defun-gsl QR-Qvec ((QR gsl-matrix-c) (tau gsl-vector-c) (v gsl-vector-c))
  "gsl_linalg_QR_Qvec"
  :documentation "Apply the matrix @math{Q} encoded in the decomposition
   (@var{QR},@var{tau}) to the vector @var{v}, storing the result @math{Q
   v} in @var{v}.  The matrix multiplication is carried out directly using
   the encoding of the Householder vectors without needing to form the full
   matrix @math{Q}."
  :after ((cl-invalidate v))
  :return-input (v))

(defun-gsl QR-Rsolve ((QR gsl-matrix-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_QR_Rsolve"
  :documentation "Solve the triangular system @math{R x = b} for
   @var{x}. It may be useful if the product @math{b' = Q^T b} has already
   been computed using QR-QTvec}."
  :after ((cl-invalidate x))
  :return-input (x))

(defun-gsl QR-Rsvx ((QR gsl-matrix-c) (x gsl-vector-c))
  "gsl_linalg_QR_Rsvx"
  :documentation "Solve the triangular system @math{R x = b} for @var{x}
  in-place. On input @var{x} should contain the right-hand side @math{b}
  and is replaced by the solution on output. This function may be useful if
  the product @math{b' = Q^T b} has already been computed using
  QR-QTvec}."
  :after ((cl-invalidate x))
  :return-input (x))

(defun-gsl QR-unpack
    ((QR gsl-matrix-c) (tau gsl-vector-c) (Q gsl-matrix-c) (R gsl-matrix-c))
  "gsl_linalg_QR_unpack"
  :documentation "Unpack the encoded @math{QR} decomposition
  (@var{QR},@var{tau}) into the matrices @var{Q} and @var{R}, where
  @var{Q} is @math{M}-by-@math{M} and @var{R} is @math{M}-by-@math{N}."
  :after ((cl-invalidate Q R))
  :return-input (Q R))

(defun-gsl QR-QRsolve
    ((Q gsl-matrix-c) (R gsl-matrix-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_QR_QRsolve"
  :documentation "Solves the system @math{R x = Q^T b} for @var{x}. It can
  be used when the @math{QR} decomposition of a matrix is available in
  unpacked form as (@var{Q}, @var{R})."
  :after ((cl-invalidate x))
  :return-input (x))

(defun-gsl QR-update
    ((Q gsl-matrix-c) (R gsl-matrix-c) (w gsl-vector-c) (v gsl-vector-c))
  "gsl_linalg_QR_update"
  :documentation "Perform a rank-1 update @math{w v^T} of the @math{QR}
  decomposition (@var{Q}, @var{R}). The update is given by @math{Q'R' = Q
  R + w v^T} where the output matrices @math{Q'} and @math{R'} are also
  orthogonal and right triangular. Note that @var{w} is destroyed by the
  update."
  :after ((cl-invalidate w Q R))
  :return-input (Q R))

(defun-gsl R-solve ((R gsl-matrix-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_R_solve"
  :documentation "Solves the triangular system @math{R x = b} for the
   @math{N}-by-@math{N} matrix @var{R}."
  :after ((cl-invalidate x))
  :return-input (x))

(defun-gsl R-svx ((R gsl-matrix-c) (x gsl-vector-c))
  "gsl_linalg_R_svx"
  :documentation "Solve the triangular system @math{R x = b} in-place. On
  input @var{x} should contain the right-hand side @math{b}, which is
  replaced by the solution on output."
  :after ((cl-invalidate x))
  :return-input (x))
