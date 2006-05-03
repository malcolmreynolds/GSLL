;********************************************************
; file:        svd.lisp                                  
; description: Singular Value Decomposition              
; date:        Tue May  2 2006 - 12:15                   
; author:      Liam Healy                                
; modified:    Wed May  3 2006 - 16:43
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; A general rectangular @math{M}-by-@math{N} matrix @math{A} has a
;;; singular value decomposition (@sc{svd}) into the product of an
;;; @math{M}-by-@math{N} orthogonal matrix @math{U}, an @math{N}-by-@math{N}
;;; diagonal matrix of singular values @math{S} and the transpose of an
;;; @math{N}-by-@math{N} orthogonal square matrix @math{V},
;;; @tex
;;; \beforedisplay
;;; $$
;;; A = U S V^T
;;; $$
;;; \afterdisplay
;;; @end tex
;;; @ifinfo

;;; @example
;;; A = U S V^T
;;; @end example
;;; @end ifinfo
;;; @noindent
;;; The singular values
;;; @c{$\sigma_i = S_{ii}$}
;;; @math{\sigma_i = S_@{ii@}} are all non-negative and are
;;; generally chosen to form a non-increasing sequence 
;;; @c{$\sigma_1 \ge \sigma_2 \ge ... \ge \sigma_N \ge 0$}
;;; @math{\sigma_1 >= \sigma_2 >= ... >= \sigma_N >= 0}.

;;; The singular value decomposition of a matrix has many practical uses.
;;; The condition number of the matrix is given by the ratio of the largest
;;; singular value to the smallest singular value. The presence of a zero
;;; singular value indicates that the matrix is singular. The number of
;;; non-zero singular values indicates the rank of the matrix.  In practice
;;; singular value decomposition of a rank-deficient matrix will not produce
;;; exact zeroes for singular values, due to finite numerical
;;; precision. Small singular values should be edited by choosing a suitable
;;; tolerance.

(defun-gsl SV-decomp ((A gsl-matrix-c) (V gsl-matrix-c) (S gsl-vector-c) (work gsl-vector-c))
  "gsl_linalg_SV_decomp"
  :documentation
  "Factorize the @math{M}-by-@math{N} matrix @var{A} into
  the singular value decomposition @math{A = U S V^T} for @c{$M \ge N$}
  @math{M >= N}.  On output the matrix @var{A} is replaced by
  @math{U}. The diagonal elements of the singular value matrix @math{S}
  are stored in the vector @var{S}. The singular values are non-negative
  and form a non-increasing sequence from @math{S_1} to @math{S_N}. The
  matrix @var{V} contains the elements of @math{V} in untransposed
  form. To form the product @math{U S V^T} it is necessary to take the
  transpose of @var{V}.  A workspace of length @var{N} is required in
  @var{work}.
  This routine uses the Golub-Reinsch SVD algorithm."
  :invalidate (A)
  :return-input (A))

(defun-gsl SV-decomp-mod
    ((A gsl-matrix-c) (X gsl-matrix-c) (V gsl-matrix-c) (S gsl-vector-c) (work gsl-vector-c))
  "gsl_linalg_SV_decomp_mod"
  :documentation
  "The SVD using the modified Golub-Reinsch algorithm, which is faster for @c{$M \gg N$}
  @math{M>>N}.  It requires the vector @var{work} of length @var{N} and the
  @math{N}-by-@math{N} matrix @var{X} as additional working space."
  :invalidate (A)
  :return-input (A))

(defun-gsl SV-decomp-jacobi ((A gsl-matrix-c) (V gsl-matrix-c) (S gsl-vector-c))
  "gsl_linalg_SV_decomp_jacobi"
  :documentation "The SVD of the @math{M}-by-@math{N} matrix @var{A}
   using one-sided Jacobi orthogonalization for @c{$M \ge N$} 
   @math{M >= N}.  The Jacobi method can compute singular values to higher
   relative accuracy than Golub-Reinsch algorithms (see references for
   details)."
  :invalidate (A)
  :return-input (A))

(defun-gsl SV-solve
    ((U gsl-matrix-c) (V gsl-matrix-c) (S gsl-vector-c) (b gsl-vector-c) (x gsl-vector-c))
  "gsl_linalg_SV_solve"
  :documentation "Solve the system @math{A x = b} using the singular value
   decomposition (@var{U}, @var{S}, @var{V}) of @math{A} given by #'SV-decomp.

   Only non-zero singular values are used in computing the solution. The
   parts of the solution corresponding to singular values of zero are
   ignored.  Other singular values can be edited out by setting them to
   zero before calling this function. 

   In the over-determined case where @var{A} has more rows than columns the
   system is solved in the least squares sense, returning the solution
   @var{x} which minimizes @math{||A x - b||_2}."
  :invalidate (x)
  :return-input (x))
