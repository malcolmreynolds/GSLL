;********************************************************
; file:        ode-system.lisp      
; description: ODE system setup
; date:        Sun Apr 15 2007 - 14:19                   
; author:      Liam Healy                                
; modified:    Mon Sep 24 2007 - 21:33
;********************************************************
;;; $Id: $

(in-package :gsl)

(cffi:defcstruct ode-system		; gsl_odeiv_system
  ;; See /usr/include/gsl/gsl_odeiv.h
  "The definition of an ordinary differential equation system for GSL."
  (function :pointer)
  (jacobian :pointer)
  (dimension :size)
  (parameters :pointer))

(export 'def-ode-function 'def-jacobian-function)

(defmacro def-ode-function (name time dependent derivatives &body body)
  "Define a function that will evaluate the right-hand sides (derivatives)
   defining a set of ordinary differential equations (ODE).
   The function should take as input the time (a double-float) and
   dependent variables (a vector of double-floats) and fill the
   derivatives vector with double-floats.  It may refer to elements
   of these vectors (arrays) using the macro double-to-cl.
   This function may be passed to the GSL ODE integrators.
   Parameters (non integration variables) may be passed by
   using a lexical closure."
  `(cffi:defcallback ,name :int
    ((,time :double)
     (,dependent :pointer)
     (,derivatives :pointer)
     (params :pointer))
    (declare (ignore params) (ignorable ,time))
    ,@body
    ;; Any errors or warnings should be signalled on the CL side;
    ;; if the function completes, we will always return success.
    (cffi:foreign-enum-value 'gsl-errorno :SUCCESS)))

#|
(def-ode-function foo time y dydt
  (setf (double-to-cl dydt 0) (- (double-to-cl y 1))
	(double-to-cl dydt 1) (double-to-cl y 0)))
|#

(defmacro def-jacobian-function (name time dependent dfdy dfdt &body body)
  "Define a function that will evaluate the Jacobian (partial derivative)
   of the set of ordinary differential equations (ODE).
   The function should take as input the time (a double-float) and
   dependent variables (a vector of double-floats) and fill the
   dfdy matrix and dfdt vector with double-floats.
   It may refer to elements
   of these vectors (arrays) using the macro double-to-cl.
   This function may be passed to the GSL ODE integrators.
   Parameters (non integration variables) may be passed by
   using a lexical closure."
  `(cffi:defcallback ,name :int
    ((,time :double)
     (,dependent :pointer)
     (,dfdy :pointer)		; This is a vector but should be CL array
     (,dfdt :pointer)
     (params :pointer))
    (declare (ignore params) (ignorable ,time))
    ,@body
    ;; Any errors or warnings should be signalled on the CL side;
    ;; if the function completes, we will always return success.
    (cffi:foreign-enum-value 'gsl-errorno :SUCCESS)))
