;********************************************************
; file:        ode-example.lisp                          
; description: Example ODE                               
; date:        Sat Sep 29 2007 - 17:49                   
; author:      Liam Healy                                
; modified:    Sat Sep 29 2007 - 18:51
;********************************************************
;;; $Id: $

(in-package :gsl)

(def-ode-function vanderpol-function (time y dydt)
  (declare (special mu))
  (setf (double-to-cl dydt 0) (double-to-cl y 1)
	(double-to-cl dydt 1)
	(- (- (double-to-cl y 0))
	   (* mu (double-to-cl y 1)
	      (- (* (double-to-cl y 0) (double-to-cl y 0)) 1)))))

(def-jacobian-function vanderpol-jacobian (time y dfdy dfdt)
  (declare (special mu))
  (setf (double-to-cl dfdt 0) 0.0d0
	(double-to-cl dfdt 1) 0.0d0
	(double-to-cl dfdy 0) 0.0d0
	(double-to-cl dfdy 1) 1.0d0
	(double-to-cl dfdy 2)
	(- (* -2 mu (double-to-cl y 0) (double-to-cl y 1)) 1.0d0)
	(double-to-cl dfdy 3)
	(* -1 mu (- (* (double-to-cl y 0) (double-to-cl y 0)) 1.0d0))))

(let ((stepper (step-allocate *step-rk8pd* 2)
	
	(step-free stepper))))
