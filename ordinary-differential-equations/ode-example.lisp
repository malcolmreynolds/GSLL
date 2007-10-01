;********************************************************
; file:        ode-example.lisp                          
; description: Example ODE                               
; date:        Sat Sep 29 2007 - 17:49                   
; author:      Liam Healy                                
; modified:    Sun Sep 30 2007 - 16:58
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

(defun integrate-vanderpol (max-time &optional (step-size 1.0d-6))
  (let ((stepper (step-allocate *step-rk8pd* 2))
	(control (new-y-control 1.0d-6 0.0d0))
	(evolve (allocate-evolution 2))
	(mu 10.0d0))
    (declare (special mu))
    (cffi:with-foreign-objects
	((vanderpol 'ode-system) (dependent :double 2))
      (setf
       (cffi:foreign-slot-value vanderpol 'ode-system 'function)
       (cffi:callback vanderpol-function)
       (cffi:foreign-slot-value vanderpol 'ode-system 'jacobian)
       (cffi:callback vanderpol-jacobian)
       (cffi:foreign-slot-value vanderpol 'ode-system 'dimension)
       2
       (cffi:foreign-slot-value vanderpol 'ode-system 'parameters)
       (cffi:null-pointer)
       (double-to-cl dependent 0) 1.0d0
       (double-to-cl dependent 1) 0.0d0)
      (loop for time from 0.0d0 below max-time
	    do
	    (apply-evolution
	     evolve control stepper vanderpol
	     time max-time step-size dependent)
	    (format t "~&~d ~d ~d"
		    time
		    (double-to-cl dependent 0)
		    (double-to-cl dependent 1))))
    (free-evolution evolve)
    (free-control control)
    (step-free stepper)))
