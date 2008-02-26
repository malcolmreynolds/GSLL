;; Householder Transformations
;; Liam Healy, Wed May 10 2006 - 10:03
;; Time-stamp: <2008-02-17 11:00:45EST householder.lisp>
;; $Id$

;;; For householder-transform, it would be nice to be able to pick the
;;; order of arguments returned (reverse).

(in-package :gsl)

;;;;****************************************************************************
;;;; Householder transformations
;;;;****************************************************************************

;;; FDL
;;; A Householder transformation is a rank-1 modification of the identity
;;; matrix which can be used to zero out selected elements of a vector.  A
;;; Householder matrix P takes the form,
;;; P = I - tau v v^T
;;; where v is a vector (called the Householder vector) and
;;; tau = 2/(v^T v).  The functions described in this section use the
;;; rank-1 structure of the Householder matrix to create and apply
;;; Householder transformations efficiently.

(defmfun householder-transform (v)
  "gsl_linalg_householder_transform"
  (((pointer v) gsl-vector-c))
  :invalidate (v)
  :c-return (ret :double)
  :return (v ret)
  :documentation			; FDL
  "Prepare a Householder transformation P = I - tau v v^T
  which can be used to zero all the elements of the input vector except
  the first.  Returned values are the transformation, which is stored
  in the vector v, and the scalar tau.")

(defmfun householder-HM (tau v A)
  "gsl_linalg_householder_hm"
  ((tau :double) ((pointer v) gsl-vector-c) ((pointer A) gsl-matrix-c))
  :invalidate (A)
  :documentation			; FDL
  "Apply the Householder matrix P defined by the
  scalar tau and the vector v to the left-hand side of the
  matrix A.  On output the result P A is stored in A.")

(defmfun householder-MH (tau v A)
  "gsl_linalg_householder_mh"
  ((tau :double) ((pointer v) gsl-vector-c) ((pointer A) gsl-matrix-c))
  :invalidate (A)
  :documentation			; FDL
  "Apply the Householder matrix P defined by the
  scalar tau and the vector v to the right-hand side of the
  matrix A.  On output the result A P is stored in A.")

(defmfun householder-Hv (tau v w)
  "gsl_linalg_householder_hv"
  ((tau :double) ((pointer v) gsl-vector-c) ((pointer w) gsl-vector-c))
  :documentation			; FDL
  "Apply the Householder transformation P defined by
  the scalar tau and the vector v to the vector w.  On
  output the result P w is stored in w."
  :invalidate (w))

;;;;****************************************************************************
;;;; Householder solver for linear systems
;;;;****************************************************************************

(defmfun householder-solve (A b x)
  "gsl_linalg_HH_solve"
  (((pointer A) gsl-matrix-c) ((pointer b) gsl-vector-c)
   ((pointer x) gsl-vector-c))
  :invalidate (x A)
  :return (x)
  :documentation			; FDL
  "Solve the system A x = b directly using
   Householder transformations. On output the solution is stored in x
   and b is not modified. The matrix A is destroyed by the
   Householder transformations.")

(defmfun householder-svx (A x)
  "gsl_linalg_HH_svx"
  (((pointer A) gsl-matrix-c) ((pointer x) gsl-vector-c))
  :invalidate (x A)
  :return (x)
  :documentation			; FDL
  "Solve the system A x = b in-place using
  Householder transformations.  On input x should contain the
  right-hand side b, which is replaced by the solution on output.  The
  matrix A is destroyed by the Householder transformations.")
