;; ODE system setup
;; Liam Healy, Sun Apr 15 2007 - 14:19
;; Time-stamp: <2009-03-29 22:54:21EDT ode-system.lisp>
;; $Id$

(in-package :gsl)

(export '(with-ode-integration))

(defmacro with-ode-integration
    ((function time step-size max-time dependent dimensions
	       &key jacobian (scalarsp t) (stepper '+step-rk8pd+)
	       (absolute-error 1.0d-6) (relative-error 0.0d0))
     &body body)
  "Environment for integration of ordinary differential equations."
  ;; Note: the case jacobian=nil is not properly handled yet; it
  ;; should put a null pointer into the struct that is passed to GSL.
  (let ((dep (make-symbol "DEP"))
	(ctime (make-symbol "CTIME"))
	(cstep (make-symbol "CSTEP")))
    `(let ((stepperobj
	    (make-ode-stepper
	     ,stepper ,dimensions ',function ',jacobian ,scalarsp))
	   (control (make-y-control ,absolute-error ,relative-error))
	   (evolve (make-ode-evolution ,dimensions))
	   (,dep
	    (make-marray 'double-float :dimensions ,dimensions))
	   (,ctime (make-marray 'double-float :dimensions 1))
	   (,cstep (make-marray 'double-float :dimensions 1)))
       (symbol-macrolet
	   ((,time (maref ,ctime 0))
	    (,step-size (maref ,cstep 0))
	    ,@(loop for symb in dependent
		 for i from 0
		 collect `(,symb (maref ,dep ,i))))
	 (flet ((next-step ()
		  (apply-evolution
		   evolve ,ctime ,dep ,cstep control stepperobj ,max-time)))
	   ,@body)))))
