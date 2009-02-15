;; Evolution functions for ODE integration.
;; Liam Healy, Sun Sep 30 2007 - 14:31
;; Time-stamp: <2009-02-14 19:29:21EST evolution.lisp>
;; $Id$

(in-package :gsl)

(defmobject ode-evolution "gsl_odeiv_evolve"
  (((first dimensions) sizet))
  "evolution for ordinary differential equations"
  :documentation
  "Make an object to advance the ODE solution."
  :superclasses (callback-included-cl)
  :ci-class-slots (ode-system marray (function jacobian) (dimension))
  :class-slots-instance (#.+callback-argument-name+)
  :initialize-suffix "reset"
  :initialize-args nil
  :singular (dimension))

(def-make-callbacks ode-evolution
    (function jacobian dimension &optional (scalars t))
  (if scalars
      `(progn
	 (defmcallback ,function
	     :success-failure
	   (:double (:double ,dimension) (:set :double ,dimension))
	   nil nil
	   ,function)
	 (defmcallback ,jacobian
	     :success-failure
	   (:double
	    (:double ,dimension)
	    (:set :double ,(expt dimension 2))
	    (:set :double ,dimension))
	   nil nil ,jacobian))
      `(progn
	 (defmcallback ,function
	     :success-failure
	   (:double :pointer :pointer)
	   nil nil
	   ,function)
	 (defmcallback ,jacobian
	     :success-failure
	   (:double :pointer :pointer :pointer)
	   nil nil ,jacobian))))

(defmfun apply-evolution
    (evolve control step time max-time step-size y)
  "gsl_odeiv_evolve_apply"
  (((mpointer evolve) :pointer) ((mpointer control) :pointer)
   ((mpointer step) :pointer)
   ((callback-struct evolve) :pointer) ((c-pointer time) :pointer)
   (max-time :double)
   ((c-pointer step-size) :pointer) ((c-pointer y) :pointer))
  :inputs (time step-size y)
  :outputs (time step-size y)
  :documentation			; FDL
  "Advance the system (e, dydt) from time
   and position y using the stepping function step.
   The new time and position are stored in time and y on output.
   The initial step-size supplied, but this will be modified
   using the control function to achieve the appropriate error
   bound if necessary.  The routine may make several calls to step in
   order to determine the optimum step-size. If the step-size has been
   changed the value of step-size will be modified on output.  The maximum
   time max-time is guaranteed not to be exceeded by the time-step.  On the
   final time-step the value of time will be set to t1 exactly.")

