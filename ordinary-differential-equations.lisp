;********************************************************
; file:        ordinary-differential-equations.lisp      
; description: ODE initial value problems                
; date:        Sun Apr 15 2007 - 14:19                   
; author:      Liam Healy                                
; modified:    Sat Sep 15 2007 - 19:24
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl 
    "gsl_odeiv_system"

)

(export 'def-ode-function)
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
     (,dependent :pointer)		; This needs to be a C array
     (,derivatives :pointer)		; This needs to be a C array
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
