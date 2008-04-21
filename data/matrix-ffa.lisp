;; Matrices
;; Liam Healy 2008-04-15 21:57:52EDT matrix-ffa.lisp
;; Time-stamp: <2008-04-15 22:31:09EDT matrix-ffa.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Matrix structure and CL object
;;;;****************************************************************************

(defclass matrix (gsl-data)
  ()
  (:documentation "GSL matrices."))

;;; Define all the mvector subclasses that are supported by FFA
#.(data-defclass 'matrix 'matrix)

(defmfun set-all ((object matrix) value)
  "gsl_matrix_set_all"
  (((mpointer object) :pointer) (value :c-base-type))
  :category matrix
  :outputs (object)
  :c-return :void
  :documentation "Set all elements to the value.")
