;; Generic functions for optimization
;; Liam Healy 2009-01-03 12:59:07EST generic.lisp
;; Time-stamp: <2009-01-24 09:11:55EST generic.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Generic functions for solve-minimize-fit objects
;;;;****************************************************************************

(defgeneric iterate (object)
  (:documentation "Take the next iteration step for this object."))

(defgeneric solution (object)
  (:documentation
   "The current value of the independent variable(s) that solves this object."))

(defgeneric function-value (object)
  (:documentation
   "The current value of the function that solves this object."))

(defgeneric last-step (object)
  (:documentation			; FDL
   "The last step dx taken by the solver.")) 

;;;;****************************************************************************
;;;; Structures and macros for function definition
;;;;****************************************************************************

(cffi:defcstruct gsl-mfunction
  ;; See /usr/include/gsl/gsl_multiroots.h
  "The definition of a function for multiroot finding in GSL."
  (function :pointer)
  (dimensions sizet)
  (parameters :pointer))

(export 'def-mfunction)
(defmacro def-mfunction (name dimensions)
  "Define a function for multivariate root solving."
  `(def-single-function ,name :success-failure :double gsl-mfunction
    ,dimensions))

(cffi:defcstruct gsl-mfunction-fdf
  ;; See /usr/include/gsl/gsl_multiroots.h
  "The definition of a function and its derivatives for multiroot
   finding in GSL."
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (dimensions sizet)
  (parameters :pointer))

