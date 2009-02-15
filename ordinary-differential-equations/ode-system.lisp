;; ODE system setup
;; Liam Healy, Sun Apr 15 2007 - 14:19
;; Time-stamp: <2009-02-15 09:09:55EST ode-system.lisp>
;; $Id$

(in-package :gsl)

(cffi:defcstruct ode-system		; gsl_odeiv_system
  ;; See /usr/include/gsl/gsl_odeiv.h
  "The definition of an ordinary differential equation system for GSL."
  (function :pointer)
  (jacobian :pointer)
  (dimension sizet)
  (parameters :pointer))

(export '(with-ode-integration))

(defmacro with-ode-integration
    ((function time step-size max-time
	       dependent dimensions &optional (stepper '*step-rk8pd*)
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
