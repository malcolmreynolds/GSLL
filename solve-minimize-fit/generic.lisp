;; Generic functions for optimization
;; Liam Healy 2009-01-03 12:59:07EST generic.lisp
;; Time-stamp: <2009-04-04 22:20:38EDT generic.lisp>
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


