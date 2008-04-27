;; Functions for both vectors and matrices.
;; Liam Healy 2008-04-26 20:48:44EDT both.lisp
;; Time-stamp: <2008-04-26 22:52:05EDT both.lisp>
;; $Id$

(in-package :gsl)

(defmfun set-all ((object both) value)
  ("gsl_" :category :type "_set_all")
  (((mpointer object) :pointer) (value :element-c-type))
  :definition :generic
  :inputs (object)
  :outputs (object)
  :c-return :void
  :documentation "Set all elements to the value.")

(defmfun set-zero ((object both))
  ("gsl_"  :category :type "_set_zero")
  (((mpointer object) :pointer))
  :definition :generic
  :inputs (object)
  :outputs (object)
  :c-return :void
  :documentation "Set all elements to 0.")

(defmfun copy ((destination both) (source both))
  ("gsl_" :category :type "_memcpy")
  (((mpointer destination) :pointer) ((mpointer source) :pointer))
  :definition :generic
  :inputs (source)
  :outputs (destination)
  :documentation			; FDL
  "Copy the elements of the source into the
   destination.  The two must have the same size.")

(defmfun swap ((a both) (b both))
  ("gsl_" :category :type "_swap")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :inputs (a b)
  :outputs (a b)
  :documentation			; FDL
  "Exchange the elements of a and b
   by copying.  The two must have the same dimensions.")

(defmfun m+ ((a both) (b both))
  ("gsl_" :category :type "_add")
  (((mpointer a) (gsl- :category -c)) ((mpointer b) (gsl- :category -c)))
  :definition :generic
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Add the elements of b to the elements of vector a
   The two must have the same dimensions.")

