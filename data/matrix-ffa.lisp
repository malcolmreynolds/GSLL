;; Matrices
;; Liam Healy 2008-04-15 21:57:52EDT matrix-ffa.lisp
;; Time-stamp: <2008-04-27 09:24:31EDT matrix-ffa.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Matrix structure and CL object
;;;;****************************************************************************

;;; GSL-matrix definition
(cffi:defcstruct gsl-matrix-c
  (size1 size)
  (size2 size)
  (tda size)
  (data :pointer)
  (block :pointer)
  (owner :int))

(defclass matrix (gsl-data)
  ()
  (:documentation "GSL matrices."))

;;; Define all the mvector subclasses that are supported by FFA
#.(data-defclass 'matrix 'matrix)

