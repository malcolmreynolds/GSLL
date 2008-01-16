;; ODE system setup
;; Liam Healy, Sun Apr 15 2007 - 14:19
;; Time-stamp: <2008-01-14 22:58:19 liam ode-system.lisp>
;; $Id: $

(in-package :gsl)

(cffi:defcstruct ode-system		; gsl_odeiv_system
  ;; See /usr/include/gsl/gsl_odeiv.h
  "The definition of an ordinary differential equation system for GSL."
  (function :pointer)
  (jacobian :pointer)
  (dimension :size)
  (parameters :pointer))

(export '(def-ode-functions with-ode-integration))

(defmacro def-ode-functions (name jacobian dimension)
  "Setup functions for ODE integrators.
   The CL functions name and jacobian should be defined previously
   with defuns."
  ;; The function should take three arguments: time, dependent, derivatives
  ;; The latter two will be C arrays.  To reference them, use #'with-c-vector.
  ;; To make this more transparent using a normal CL function
  ;; would require transferring numbers back and forth between C and CL arrays,
  ;; which could be inefficient.
  (let ((time (make-symbol "TIME"))
	(dependent (make-symbol "DEP"))
	(derivatives (make-symbol "DERIV"))
	(dfdy (make-symbol "DFDY"))
	(dfdt (make-symbol "DFDT"))
	(params (make-symbol "PARAMS")))
    `(progn
      (cffi:defcallback ,name :int
	  ((,time :double)
	   (,dependent :pointer)
	   (,derivatives :pointer)
	   (,params :pointer))
	(declare (ignore ,params))
	(,name ,time ,dependent ,derivatives)
	success)
      ;; The function should take four arguments: time, dependent, dfdy, dfdt
      ;; The last three will be arrays.
      (cffi:defcallback ,jacobian :int
	  ((,time :double)
	   (,dependent :pointer)
	   (,dfdy :pointer)
	   (,dfdt :pointer)
	   (,params :pointer))	
	(declare (ignore ,params))
	(,name ,time ,dependent ,dfdy ,dfdt)
	success)
      ;; Assume that defcallback does not bind the variable 'name.
      (defparameter ,name (cffi:foreign-alloc 'ode-system))
      (set-slot-function ,name 'ode-system 'function ',name)
      (set-slot-function ,name 'ode-system 'jacobian ',jacobian)
      (set-structure-slot ,name 'ode-system 'dimension ,dimension)
      (set-parameters ,name 'ode-system))))

(defmacro with-ode-integration
    ((time step-size dependent dimensions &optional (stepper '*step-rk8pd*)
	   (absolute-error 1.0d-6) (relative-error 0.0d0))
     &body body)
  "Environment for integration of ordinary differential equations.
   The variables time and step-size will become C doubles in the body;
   to convert back, use double-to-cl.  The dependent variable may
   be specified as a list being the same as the first argument to
   with-c-double."
  (let ((ctime (make-symbol "CTIME"))
	(cstep (make-symbol "CSTEP")))
    `(let ((stepper (step-allocate ,stepper ,dimensions))
	   (control (new-y-control ,absolute-error ,relative-error))
	   (evolve (allocate-evolution ,dimensions)))
      (unwind-protect
	   (cffi:with-foreign-objects
	       ((,(if (listp dependent) (first dependent) dependent)
		  :double ,dimensions) (,ctime :double) (,cstep :double))
	     (setf
	      (double-to-cl ,cstep) ,step-size
	      (double-to-cl ,ctime) ,time)
	     ,(if (listp dependent)
		  `(with-c-double ,dependent
		    (symbol-macrolet ((,time ,ctime) (,step-size ,cstep))
		    ,@body))
		  `(symbol-macrolet ((,time ,ctime) (,step-size ,cstep))
		    ,@body)))
	(free-evolution evolve)
	(free-control control)
	(step-free stepper)))))

