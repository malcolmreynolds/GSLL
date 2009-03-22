;; Generic functions for optimization
;; Liam Healy 2009-01-03 12:59:07EST generic.lisp
;; Time-stamp: <2009-03-21 23:52:17EDT generic.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Generic functions for solve-minimize-fit objects
;;;;****************************************************************************

(export '(iterate solution function-value last-step))

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
  (dimension sizet)
  (parameters :pointer))

(cffi:defcstruct gsl-mfunction-fdf
  ;; See /usr/include/gsl/gsl_multiroots.h
  "The definition of a function and its derivatives for multiroot
   finding in GSL."
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (dimension sizet)
  (parameters :pointer))

;;;;****************************************************************************
;;;; Function definition
;;;;****************************************************************************

;;; Solvers that require the function and its derivative use the
;;; following structure.
(cffi:defcstruct gsl-function-fdf
  ;; See /usr/include/gsl/gsl_math.h
  "The definition of a function and its derivative for root finding in GSL."
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (parameters :pointer))
