;; Tridiagonal and Bidiagonal matrices
;; Liam Healy, Thu May  4 2006 - 15:43
;; Time-stamp: <2008-02-17 10:55:01EST diagonal.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Tridiagonal Decomposition of Real Symmetric Matrices
;;;;****************************************************************************

;;; FDL
;;; A symmetric matrix A can be factorized by similarity
;;; transformations into the form 
;;; A = Q T Q^T
;;; where Q is an orthogonal matrix and T is a symmetric
;;; tridiagonal matrix.

(defmfun symmtd-decomp (A tau)
  "gsl_linalg_symmtd_decomp"
  (((pointer A) gsl-matrix-c) ((pointer tau) gsl-vector-c))
  :invalidate (A tau)
  :documentation			; FDL
  "Factorizes the symmetric square matrix A into the
   symmetric tridiagonal decomposition Q T Q^T.  On output the
   diagonal and subdiagonal part of the input matrix A contain the
   tridiagonal matrix T.  The remaining lower triangular part of the
   input matrix contains the Householder vectors which, together with the
   Householder coefficients tau, encode the orthogonal matrix
   Q.  This storage scheme is the same as used by lapack.  The
   upper triangular part of A is not referenced.")

(defmfun symmtd-unpack (A tau Q diag subdiag)
  "gsl_linalg_symmtd_unpack"
  (((pointer A) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer Q) gsl-matrix-c) ((pointer diag) gsl-vector-c)
   ((pointer subdiag) gsl-vector-c))
  :invalidate (Q diag subdiag)
  :documentation			; FDL
  "Unpacks the encoded symmetric tridiagonal decomposition
  (A, tau) obtained from #'symmtd-decomp into
  the orthogonal matrix Q, the vector of diagonal elements diag
  and the vector of subdiagonal elements subdiag.")

(defmfun symmtd-unpack-T (A diag subdiag)
  "gsl_linalg_symmtd_unpack_T"
  (((pointer A) gsl-matrix-c) ((pointer diag) gsl-vector-c)
   ((pointer subdiag) gsl-vector-c))
  :invalidate (diag subdiag)
  :documentation			; FDL
  "Unpack the diagonal and subdiagonal of the encoded
   symmetric tridiagonal decomposition (A, tau) obtained from
   #'symmtd-decomp into the vectors diag and subdiag.")

;;;;****************************************************************************
;;;; Tridiagonal Decomposition of Hermitian Matrices
;;;;****************************************************************************

;;; FDL
;;; A hermitian matrix A can be factorized by similarity
;;; transformations into the form
;;; A = U T U^T where U is a unitary
;;; matrix and T is a real symmetric tridiagonal matrix.

(defmfun hermitian-decomp (A tau)
  "gsl_linalg_hermtd_decomp"
  (((pointer A) gsl-matrix-c) ((pointer tau) gsl-vector-c))
  :invalidate (A tau)
  :documentation			; FDL
  "This function factorizes the hermitian matrix A
   into the symmetric tridiagonal decomposition U T U^T.
   On output the real parts of
   the diagonal and subdiagonal part of the input matrix A contain
   the tridiagonal matrix T.  The remaining lower triangular part of
   the input matrix contains the Householder vectors which, together with
   the Householder coefficients tau, encode the orthogonal matrix
   Q. This storage scheme is the same as used by lapack.  The
   upper triangular part of A and imaginary parts of the diagonal are
   not referenced.")

;;; GSL doc or arglist incorrect? Q->U
(defmfun hermitian-unpack (A tau U diag subdiag)
  "gsl_linalg_hermtd_unpack"
  (((pointer A) gsl-matrix-c) ((pointer tau) gsl-vector-c)
   ((pointer U) gsl-matrix-c)
   ((pointer diag) gsl-vector-c) ((pointer subdiag) gsl-vector-c))
  :invalidate (U diag subdiag)
  :documentation			; FDL
  "Unpacks the encoded tridiagonal decomposition (A, tau)
   obtained from hermitian-decomp into the
   unitary matrix U, the real vector of diagonal elements diag and
   the real vector of subdiagonal elements subdiag. ")

(defmfun hermitian-unpack-T (A diag subdiag)
  "gsl_linalg_hermtd_unpack_T"
  (((pointer A) gsl-matrix-c)
   ((pointer diag) gsl-vector-c) ((pointer subdiag) gsl-vector-c))
  :invalidate (diag subdiag)
  :documentation			; FDL
  "Unpack the diagonal and subdiagonal of the encoded
  tridiagonal decomposition (A, tau) obtained from the
  hermitian-decomp into the real vectors diag and subdiag.")

;;;;****************************************************************************
;;;; Bidiagonal
;;;;****************************************************************************

;;; FDL
;;; A general matrix A can be factorized by similarity
;;; transformations into the form A = U B V^T
;;; where U and V are orthogonal matrices and B is a
;;; N-by-N bidiagonal matrix with non-zero entries only on the
;;; diagonal and superdiagonal.  The size of U is M-by-N
;;; and the size of V is N-by-N.

(defmfun bidiagonal-decomp (A tau-U tau-V)
  "gsl_linalg_bidiag_decomp"
  (((pointer A) gsl-matrix-c) ((pointer tau-U) gsl-vector-c)
   ((pointer tau-V) gsl-vector-c))
  :invalidate (A tau-U tau-V)
  :documentation			; FDL
  "Factorize the M-by-N matrix A into
   bidiagonal form U B V^T.  The diagonal and superdiagonal of the
   matrix B are stored in the diagonal and superdiagonal of A,
   The orthogonal matrices U and V are stored as compressed
   Householder vectors in the remaining elements of A.  The
   Householder coefficients are stored in the vectors tau-U and
   tau-V.  The length of tau-U must equal the number of
   elements in the diagonal of A and the length of tau-V should
   be one element shorter.")

(defmfun bidiagonal-unpack (A tau-U U tau-V V diag subdiag)
  "gsl_linalg_bidiag_unpack"
  (((pointer A) gsl-matrix-c)
   ((pointer tau-U) gsl-vector-c) ((pointer U) gsl-matrix-c)
   ((pointer tau-V) gsl-vector-c) ((pointer V) gsl-matrix-c)
   ((pointer diag) gsl-vector-c) ((pointer subdiag) gsl-vector-c))
  :invalidate (U V diag subdiag)
  :documentation			; FDL
  "Unpack the bidiagonal decomposition of A given by
   #'bidiagonal-decomp (A, tau-U, tau-V)
   into the separate orthogonal matrices U, V, and the diagonal
   vector diag and superdiagonal superdiag.  Note that U
   is stored as a compact M-by-N orthogonal matrix satisfying
   U^T U = I for efficiency.")

;;; There is no 'diag and 'superdiag given for arguments to this function
(defmfun bidiagonal-unpack2 (A tau-U tau-V V)
  "gsl_linalg_bidiag_unpack2"
  (((pointer A) gsl-matrix-c) ((pointer tau-U) gsl-vector-c)
   ((pointer tau-V) gsl-vector-c) ((pointer V) gsl-matrix-c))
  :invalidate (A V)
  :documentation			; FDL
  "Unpack the bidiagonal decomposition of A given by
   #'bidiagonal-decomp (A, tau-U, tau-V)
   into the separate orthogonal matrices U, V and the diagonal
   vector diag and superdiagonal superdiag.  The matrix U
   is stored in-place in A.")

(defmfun bidiagonal-unpack-B (A diag superdiag)
  "gsl_linalg_bidiag_unpack_B"
  (((pointer A) gsl-matrix-c) ((pointer diag) gsl-vector-c)
   ((pointer superdiag) gsl-vector-c))
  :documentation			; FDL
  "Unpack the diagonal and superdiagonal of the bidiagonal
  decomposition of A given by #'bidiagonal-decomp, into the diagonal
  vector diag and superdiagonal vector superdiag."
  :invalidate (diag superdiag))

;;;;****************************************************************************
;;;; Tridiagonal Systems
;;;;****************************************************************************

(defmfun solve-tridiagonal (diag e f b x)
  "gsl_linalg_solve_tridiag"
  (((pointer diag) gsl-vector-c) ((pointer e) gsl-vector-c)
   ((pointer f) gsl-vector-c) ((pointer b) gsl-vector-c)
   ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the general N-by-N system A x = b where A is tridiagonal
   (N >= 2). The super-diagonal and
   sub-diagonal vectors e and f must be one element shorter
   than the diagonal vector diag.  The form of A for the 4-by-4
   case is
   A = ( d_0 e_0  0   0  )
       ( f_0 d_1 e_1  0  )
       (  0  f_1 d_2 e_2 )
       (  0   0  f_2 d_3 ).")

(defmfun solve-symmetric-tridiagonal (diag e b x)
  "gsl_linalg_solve_symm_tridiag"
  (((pointer diag) gsl-vector-c) ((pointer e) gsl-vector-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the general N-by-N system A x = b where A is
   symmetric tridiagonal (N >= 2).  The off-diagonal vector
   e must be one element shorter than the diagonal vector diag.
   The form of A for the 4-by-4 case is
    A = ( d_0 e_0  0   0  )
        ( e_0 d_1 e_1  0  )
        (  0  e_1 d_2 e_2 )
        (  0   0  e_2 d_3 ).")

(defmfun solve-cyclic-tridiagonal (diag e f b x)
  "gsl_linalg_solve_cyc_tridiag"
  (((pointer diag) gsl-vector-c) ((pointer e) gsl-vector-c)
   ((pointer f) gsl-vector-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the general N-by-N system A x = b where A is cyclic
   tridiagonal (N >= 3).  The cyclic super-diagonal and
   sub-diagonal vectors e and f must have the same number of
   elements as the diagonal vector diag.  The form of A for the
   4-by-4 case is
     A = ( d_0 e_0  0  f_3 )
         ( f_0 d_1 e_1  0  )
         (  0  f_1 d_2 e_2 )
         ( e_3  0  f_2 d_3 ).")

(defmfun solve-symmetric-cyclic-tridiagonal (diag e b x)
  "gsl_linalg_solve_symm_cyc_tridiag"
  (((pointer diag) gsl-vector-c) ((pointer e) gsl-vector-c)
   ((pointer b) gsl-vector-c) ((pointer x) gsl-vector-c))
  :invalidate (x)
  :documentation			; FDL
  "Solve the general N-by-N system A x = b where A is symmetric
   cyclic tridiagonal (N >= 3).  The cyclic
   off-diagonal vector e must have the same number of elements as the
   diagonal vector diag.  The form of A for the 4-by-4 case is
   shown below,
   A = ( d_0 e_0  0  e_3 )
       ( e_0 d_1 e_1  0  )
       (  0  e_1 d_2 e_2 )
       ( e_3  0  e_2 d_3 )")
