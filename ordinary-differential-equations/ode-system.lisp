;; ODE system setup
;; Liam Healy, Sun Apr 15 2007 - 14:19
;; Time-stamp: <2009-01-24 18:23:33EST ode-system.lisp>
;; $Id$

(in-package :gsl)

(cffi:defcstruct ode-system		; gsl_odeiv_system
  ;; See /usr/include/gsl/gsl_odeiv.h
  "The definition of an ordinary differential equation system for GSL."
  (function :pointer)
  (jacobian :pointer)
  (dimension sizet)
  (parameters :pointer))

(export '(make-ode-functions with-ode-integration))

(defmacro make-ode-functions (name jacobian dimension)
  "Setup functions for ODE integrators.  The variable name is used as the name of the 
   The CL functions name and jacobian should be defined previously
   with defuns.
   The function and Jacobian arguments are the same: time and
   dimension dependent variables as scalars.
   The function returns dimension multiple values corresponding to
   the derivatives of the dependent variables, f(t).
   The Jacobian returns dimension^2 values corresponding to the
   partial derivatives of each of the functions f with respect to each
   of the dependent variables y, and dimension values corresponding to
   the derivatives of f with respect to t."
  ;; set return values
  ;; Possible future improvements: take/set arrays easily, allow lambdas instead of named functions.
  (with-unique-names (solverfn solverdf)
    `(progn
       (defmcallback
	   ,solverfn :success-failure
	 (:double (:double ,dimension) (:set :double ,dimension)) nil nil ,name)
       (defmcallback
	   ,solverdf :success-failure
	 (:double (:double ,dimension) (:set :double ,(expt dimension 2))
		  (:set :double ,dimension)) nil nil ,jacobian)
       (defcbstruct (,solverfn function ,solverdf jacobian) ode-system
	 ((dimension ,dimension))))))

(defmacro with-ode-integration
    ((time step-size dependent dimensions &optional (stepper '*step-rk8pd*)
	   (absolute-error 1.0d-6) (relative-error 0.0d0))
     &body body)
  "Environment for integration of ordinary differential equations.
   The variables time and step-size will become C doubles in the body;
   to convert back, use #'dcref  The dependent variable may
   be specified as a list being the same as the first argument to
   with-c-double."
  (let ((ctime (make-symbol "CTIME"))
	(cstep (make-symbol "CSTEP")))
    `(let ((stepper (make-ode-stepper ,stepper ,dimensions))
	   (control (new-y-control ,absolute-error ,relative-error))
	   (evolve (make-ode-evolution ,dimensions)))
       (unwind-protect
	    (cffi:with-foreign-objects
		((,(if (listp dependent) (first dependent) dependent)
		   :double ,dimensions) (,ctime :double) (,cstep :double))
	      (setf
	       (dcref ,cstep) ,step-size
	       (dcref ,ctime) ,time)
	      ,(if (listp dependent)
		   `(with-c-double ,dependent
		      (symbol-macrolet ((,time ,ctime) (,step-size ,cstep))
			,@body))
		   `(symbol-macrolet ((,time ,ctime) (,step-size ,cstep))
		      ,@body)))
	 (free-control control)))))
