;; ODE system setup
;; Liam Healy, Sun Apr 15 2007 - 14:19
;; Time-stamp: <2009-03-22 19:54:05EDT ode-system.lisp>
;; $Id$

(in-package :gsl)

(export '(with-ode-integration))

(defmacro with-ode-integration
    ((function time step-size max-time
	       dependent dimensions &optional (stepper '+step-rk8pd+)
	       (absolute-error 1.0d-6) (relative-error 0.0d0))
     &body body)
  "Environment for integration of ordinary differential equations."
  (let ((dep (make-symbol "DEP"))
	(ctime (make-symbol "CTIME"))
	(cstep (make-symbol "CSTEP")))
    `(let ((stepperobj (make-ode-stepper ,stepper ,dimensions ',function))
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
