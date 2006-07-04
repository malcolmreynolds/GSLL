;********************************************************
; file:        householder.lisp                          
; description: Householder Transformations
; date:        Wed May 10 2006 - 10:03                   
; author:      Liam Healy                                
; modified:    Mon Jul  3 2006 - 23:41
;********************************************************
;;; $Id: $

;;; For householder-transform, it would be nice to be able to pick the
;;; order of arguments returned (reverse).

(in-package :gsl)

;;;;****************************************************************************
;;;; Householder transformations
;;;;****************************************************************************

;;; A Householder transformation is a rank-1 modification of the identity
;;; matrix which can be used to zero out selected elements of a vector.  A
;;; Householder matrix @math{P} takes the form,
;;; P = I - \tau v v^T
;;; where @math{v} is a vector (called the @dfn{Householder vector}) and
;;; @math{\tau = 2/(v^T v)}.  The functions described in this section use the
;;; rank-1 structure of the Householder matrix to create and apply
;;; Householder transformations efficiently.

(defun-gsl householder-transform (v)
  "gsl_linalg_householder_transform"
  (((pointer v) gsl-vector-c))
  :documentation "Prepare a Householder transformation @math{P = I - \tau v
  v^T} which can be used to zero all the elements of the input vector except
  the first.  Returned values are the transformation, which is stored
  in the vector @var{v}, and the scalar @math{\tau}."
  :invalidate (v)
  :c-return (ret :double)
  :return (v ret))

(defun-gsl householder-HM (tau v A)
  "gsl_linalg_householder_hm"
  ((tau :double) ((pointer v) gsl-vector-c) ((pointer A) gsl-matrix-c))
  :documentation "Apply the Householder matrix @math{P} defined by the
  scalar @var{tau} and the vector @var{v} to the left-hand side of the
  matrix @var{A}. On output the result @math{P A} is stored in @var{A}."
  :invalidate (A))

(defun-gsl householder-MH (tau v A)
  "gsl_linalg_householder_mh"
  ((tau :double) ((pointer v) gsl-vector-c) ((pointer A) gsl-matrix-c))
  :documentation "Apply the Householder matrix @math{P} defined by the
  scalar @var{tau} and the vector @var{v} to the right-hand side of the
  matrix @var{A}. On output the result @math{A P} is stored in @var{A}."
  :invalidate (A))

(defun-gsl householder-Hv (tau v w)
  "gsl_linalg_householder_hv"
  ((tau :double) ((pointer v) gsl-vector-c) ((pointer w) gsl-vector-c))
  :documentation "Apply the Householder transformation @math{P} defined by
  the scalar @var{tau} and the vector @var{v} to the vector @var{w}.  On
  output the result @math{P w} is stored in @var{w}."
  :invalidate (w))

;;;;****************************************************************************
;;;; Householder solver for linear systems
;;;;****************************************************************************

(defun-gsl householder-solve (A b x)
  "gsl_linalg_HH_solve"
  (((pointer A) gsl-matrix-c) ((pointer b) gsl-vector-c)
   ((pointer x) gsl-vector-c))
  :documentation "Solve the system @math{A x = b} directly using
   Householder transformations. On output the solution is stored in @var{x}
   and @var{b} is not modified. The matrix @var{A} is destroyed by the
   Householder transformations."
  :invalidate (x A)
  :return (x))

(defun-gsl householder-svx (A x)
  "gsl_linalg_HH_svx"
  (((pointer A) gsl-matrix-c) ((pointer x) gsl-vector-c))
  :documentation "Solve the system @math{A x = b} in-place using
  Householder transformations.  On input @var{x} should contain the
  right-hand side @math{b}, which is replaced by the solution on output.  The
  matrix @var{A} is destroyed by the Householder transformations."
  :invalidate (x A)
  :return (x))
