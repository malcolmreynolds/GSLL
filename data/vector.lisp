;; Vectors
;; Liam Healy 2008-04-13 09:39:02EDT vector.lisp
;; Time-stamp: <2008-12-28 16:55:05EST vector.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Vector structure, CL object, and allocation
;;;;****************************************************************************

(defclass mvector (marray)
  ()
  (:documentation "GSL vectors."))

;;; GSL-vector definition
(cffi:defcstruct gsl-vector-c
  (size sizet)
  (stride sizet)
  (data :pointer)
  (block :pointer)
  (owner :int))

;;; Define all supported mvector subclasses
#.(data-defclass 'vector 'mvector)

;;;;****************************************************************************
;;;; Function definitions
;;;;****************************************************************************

(defmfun set-basis ((object vector) index)
  ("gsl_" :category :type "_set_basis")
  (((mpointer object) :pointer) (index sizet))
  :definition :generic
  :inputs (object)
  :outputs (object)
  :return (object)
  :outputs (object)
  :documentation			; FDL
  "Set the index element to 1, and the rest to 0.")

(defmfun swap-elements ((vec vector) i j)
  ("gsl_" :category :type "_swap_elements")
  (((mpointer vec) :pointer) (i sizet) (j sizet))
  :definition :generic
  :inputs (vec)
  :outputs (vec)
  :return (vec)
  :documentation			; FDL
  "Exchange the i-th and j-th elements of the vector vec in-place.")

(defmfun vector-reverse ((vec vector))
  ("gsl_" :category :type "_reverse")
  (((mpointer vec) :pointer))
  :definition :generic
  :inputs (vec)
  :outputs (vec)
  :return (vec)
  :documentation			; FDL
  "Reverse the order of the elements of the vector vec.")
