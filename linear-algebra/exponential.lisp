;; Exponential of a matrix
;; Liam Healy 2008-08-10 17:25:35EDT exponential.lisp
;; Time-stamp: <2008-08-10 18:27:04EDT exponential.lisp>
;; $Id: $

(in-package :gsl)

;;; No documentation
(defmfun matrix-exponential (matrix exponential &optional (mode :double))
  "gsl_linalg_exponential_ss"
  (((mpointer matrix) :pointer) ((mpointer exponential) :pointer)
   (mode sf-mode))
  :inputs (matrix)
  :outputs (exponential)
  :return (exponential)
  :documentation
  "Calculate the matrix exponential by the scaling and
   squaring method described in Moler + Van Loan,
   SIAM Rev 20, 801 (1978).  The matrix to be exponentiated
   is matrix, the returned exponential is exponential.
   The mode argument allows
   choosing an optimal strategy, from the table
   given in the paper, for a given precision.")

#|
;;; A matrix of the form [[0, 1], [-1, 0]]
;;; when exponentiated gives [[cos x,  sin x], [-sin x,  cos x]]
(letm ((mat (matrix-double-float (a (0.0d0 1.0d0) (-1.0d0 0.0d0))))
       (exp (matrix-double-float '(2 2))))
  (cl-array (matrix-exponential mat exp)))
#2A((0.5403023058681384d0 0.841470984807895d0)
    (-0.841470984807895d0 0.5403023058681384d0))
|#
