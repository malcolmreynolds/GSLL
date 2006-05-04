;********************************************************
; file:        tridiagonal.lisp                          
; description: Tridiagonal and Bidiagonal matrices
; date:        Thu May  4 2006 - 15:43                   
; author:      Liam Healy                                
; modified:    Thu May  4 2006 - 17:43
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Tridiagonal Decomposition of Real Symmetric Matrices
;;;;****************************************************************************

;;; A symmetric matrix @math{A} can be factorized by similarity
;;; transformations into the form 
;;; A = Q T Q^T
;;; where @math{Q} is an orthogonal matrix and @math{T} is a symmetric
;;; tridiagonal matrix.

(defun-gsl symmtd-decomp ((A gsl-matrix-c) (tau gsl-vector-c))
  "gsl_linalg_symmtd_decomp"
  :documentation "Factorizes the symmetric square matrix @var{A} into the
   symmetric tridiagonal decomposition @math{Q T Q^T}.  On output the
   diagonal and subdiagonal part of the input matrix @var{A} contain the
   tridiagonal matrix @math{T}.  The remaining lower triangular part of the
   input matrix contains the Householder vectors which, together with the
   Householder coefficients @var{tau}, encode the orthogonal matrix
   @math{Q}. This storage scheme is the same as used by @sc{lapack}.  The
   upper triangular part of @var{A} is not referenced."
  :invalidate (A tau)
  :return-input (A tau))

(defun-gsl symmtd-unpack
    ((A gsl-matrix-c) (tau gsl-vector-c)
     (Q gsl-matrix-c) (diag gsl-vector-c) (subdiag gsl-vector-c))
  "gsl_linalg_symmtd_unpack"
  :documentation "Unpacks the encoded symmetric tridiagonal decomposition
  (@var{A}, @var{tau}) obtained from #'symmtd-decomp into
  the orthogonal matrix @var{Q}, the vector of diagonal elements @var{diag}
  and the vector of subdiagonal elements @var{subdiag}."
  :invalidate (Q diag subdiag)
  :return-input (Q diag subdiag))

(defun-gsl symmtd-unpack-T
    ((A gsl-matrix-c) (diag gsl-vector-c) (subdiag gsl-vector-c))
  "gsl_linalg_symmtd_unpack_T"
  :documentation "Unpack the diagonal and subdiagonal of the encoded
   symmetric tridiagonal decomposition (@var{A}, @var{tau}) obtained from
   #'symmtd-decomp into the vectors @var{diag} and @var{subdiag}."
  :invalidate (diag subdiag)
  :return-input (diag subdiag))

;;;;****************************************************************************
;;;; Tridiagonal Decomposition of Hermitian Matrices
;;;;****************************************************************************

;;; A hermitian matrix @math{A} can be factorized by similarity
;;; transformations into the form
;;; A = U T U^T where @math{U} is a unitary
;;; matrix and @math{T} is a real symmetric tridiagonal matrix.

;;; Deferred port until complex vectors exist.

;;;;****************************************************************************
;;;; Bidiagonal
;;;;****************************************************************************

;;; A general matrix @math{A} can be factorized by similarity
;;; transformations into the form A = U B V^T
;;; where @math{U} and @math{V} are orthogonal matrices and @math{B} is a
;;; @math{N}-by-@math{N} bidiagonal matrix with non-zero entries only on the
;;; diagonal and superdiagonal.  The size of @var{U} is @math{M}-by-@math{N}
;;; and the size of @var{V} is @math{N}-by-@math{N}.

(defun-gsl bidiagonal-decomp ((A gsl-matrix-c) (tau-U gsl-vector-c) (tau-V gsl-vector-c))
  "gsl_linalg_bidiag_decomp"
  :documentation "Factorize the @math{M}-by-@math{N} matrix @var{A} into
   bidiagonal form @math{U B V^T}.  The diagonal and superdiagonal of the
   matrix @math{B} are stored in the diagonal and superdiagonal of @var{A}.
   The orthogonal matrices @math{U} and @var{V} are stored as compressed
   Householder vectors in the remaining elements of @var{A}.  The
   Householder coefficients are stored in the vectors @var{tau_U} and
   @var{tau_V}.  The length of @var{tau_U} must equal the number of
   elements in the diagonal of @var{A} and the length of @var{tau_V} should
   be one element shorter."
  :invalidate (A tau-U tau-V)
  :return-input (A tau-U tau-V))

(defun-gsl bidiagonal-unpack
    ((A gsl-matrix-c)
     (tau-U gsl-vector-c) (U gsl-matrix-c)
     (tau-V gsl-vector-c) (V gsl-matrix-c)
     (diag gsl-vector-c) (subdiag gsl-vector-c))
  "gsl_linalg_bidiag_unpack"
  :documentation "Unpack the bidiagonal decomposition of @var{A} given by
   #'bidiagonal-decomp (@var{A}, @var{tau_U}, @var{tau_V})
   into the separate orthogonal matrices @var{U}, @var{V} and the diagonal
   vector @var{diag} and superdiagonal @var{superdiag}.  Note that @var{U}
   is stored as a compact @math{M}-by-@math{N} orthogonal matrix satisfying
   @math{U^T U = I} for efficiency."
  :invalidate (U V diag subdiag)
  :return-input (U V diag subdiag))

;;; There is no 'diag and 'superdiag given for arguments to this function
(defun-gsl bidiagonal-unpack2
        ((A gsl-matrix-c) (tau-U gsl-vector-c) (tau-V gsl-vector-c) (V gsl-matrix-c))
  "gsl_linalg_bidiag_unpack2"
  :documentation "Unpack the bidiagonal decomposition of @var{A} given by
   #'bidiagonal-decomp (@var{A}, @var{tau_U}, @var{tau_V})
   into the separate orthogonal matrices @var{U}, @var{V} and the diagonal
   vector @var{diag} and superdiagonal @var{superdiag}.  The matrix @var{U}
   is stored in-place in @var{A}."
  :invalidate (A V)
  :return-input (A V))

(defun-gsl bidiagonal-unpack-B
    ((A gsl-matrix-c) (diag gsl-vector-c) (superdiag gsl-vector-c))
  "gsl_linalg_bidiag_unpack_B"
  :documentation "Unpack the diagonal and superdiagonal of the bidiagonal
  decomposition of @var{A} given by #'bidiagonal-decomp, into the diagonal
  vector @var{diag} and superdiagonal vector @var{superdiag}."
  :invalidate (diag superdiag)
  :return-input (diag superdiag))

;;;;****************************************************************************
;;;; Tridiagonal Systems
;;;;****************************************************************************

(defun-gsl solve-tridiagonal
    ((diag gsl-vector-c) (e gsl-vector-c) (f gsl-vector-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_solve_tridiag"
  :documentation "Solve the general @math{N}-by-@math{N} system @math{A x =
   b} where @var{A} is tridiagonal (@math{N >= 2}). The super-diagonal and
   sub-diagonal vectors @var{e} and @var{f} must be one element shorter
   than the diagonal vector @var{diag}.  The form of @var{A} for the 4-by-4
   case is
   A = ( d_0 e_0  0   0  )
       ( f_0 d_1 e_1  0  )
       (  0  f_1 d_2 e_2 )
       (  0   0  f_2 d_3 )."
  :invalidate (x)
  :return-input (x))

(defun-gsl solve-symmetric-tridiagonal
    ((diag gsl-vector-c) (e gsl-vector-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_solve_symm_tridiag"
  :documentation "Solve the general @math{N}-by-@math{N} system @math{A x =
   b} where @var{A} is symmetric tridiagonal (@math{N >= 2}).  The off-diagonal vector
   @var{e} must be one element shorter than the diagonal vector @var{diag}.
   The form of @var{A} for the 4-by-4 case is
    A = ( d_0 e_0  0   0  )
        ( e_0 d_1 e_1  0  )
        (  0  e_1 d_2 e_2 )
        (  0   0  e_2 d_3 )."
  :invalidate (x)
  :return-input (x))

(defun-gsl solve-cyclic-tridiagonal
    ((diag gsl-vector-c) (e gsl-vector-c) (f gsl-vector-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_solve_cyc_tridiag"
  :documentation "Solve the general @math{N}-by-@math{N} system @math{A x =
   b} where @var{A} is cyclic tridiagonal (@math{N >= 3}).  The cyclic super-diagonal and
   sub-diagonal vectors @var{e} and @var{f} must have the same number of
   elements as the diagonal vector @var{diag}.  The form of @var{A} for the
   4-by-4 case is
     A = ( d_0 e_0  0  f_3 )
         ( f_0 d_1 e_1  0  )
         (  0  f_1 d_2 e_2 )
         ( e_3  0  f_2 d_3 )."
  :invalidate (x)
  :return-input (x))

(defun-gsl solve-symmetric-cyclic-tridiagonal
    ((diag gsl-vector-c) (e gsl-vector-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_solve_symm_cyc_tridiag"
  :documentation "Solve the general @math{N}-by-@math{N} system @math{A x =
  b} where @var{A} is symmetric cyclic tridiagonal (@math{N >= 3}).  The cyclic
  off-diagonal vector @var{e} must have the same number of elements as the
  diagonal vector @var{diag}.  The form of @var{A} for the 4-by-4 case is
  shown below,
   A = ( d_0 e_0  0  e_3 )
       ( e_0 d_1 e_1  0  )
       (  0  e_1 d_2 e_2 )
       ( e_3  0  e_2 d_3 )"
  :invalidate (x)
  :return-input (x))
