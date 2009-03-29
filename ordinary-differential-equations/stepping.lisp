;; Stepping functions for ODE systems.
;; Liam Healy, Mon Sep 24 2007 - 21:33
;; Time-stamp: <2009-03-29 12:34:49EDT stepping.lisp>
;; $Id$

;; /usr/include/gsl/gsl_odeiv.h

(in-package :gsl)

(defmobject ode-stepper "gsl_odeiv_step"
  ((type :pointer) ((first dimensions) sizet))
  "stepper for ordinary differential equations"
  :documentation
  "Make a stepper for ordinary differential equations.  The type is
   one of the GSL-supplied types defined in stepping.lisp, and
   dimensions is the number of dependent variables.  This instance
   should be reinitialized whenever the next use of it will not be a
   continuation of a previous step."
  :superclasses (callback-included-cl)
  :callbacks
  (callback ode-system
	    (dimension)
	    (function :success-failure
		      (:input :double)	; t (independent variable)
		      (:input :double :cvector dim0) ; y (dependent variables)
		      (:output :double :cvector dim0) ; dydt
		      :slug)
	    (jacobian :success-failure
		      (:input :double)	; t (independent variable)
		      (:input :double :cvector dim0) ; y (dependent variables)
		      (:output :double :cvector dim0 dim0) ; dfdy
		      (:output :double :cvector dim0)	   ; dydt
		      :slug))
  :class-slots-instance (callback)
  :initialize-suffix "reset"
  :initialize-args nil
  :singular (dimension))

(cffi:defcstruct ode-system		; gsl_odeiv_system
  "The definition of an ordinary differential equation system for GSL."
  (function :pointer)
  (jacobian :pointer)
  (dimension sizet)
  (parameters :pointer))


#|
(def-make-callbacks ode-stepper
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
|#



#|
This description applies when scalars=t:

Setup functions for ODE integrators.  The variables `function' and
`jacobian' are CL functions that define the ODE and its jacobian,
and should be defined previously with defuns.

The ODE integrator solves a system of first order differential
equations:

 d y_i / dx = f_i(x,y_1,y_2,...) with y_i=y_1, y_2, ...

The arguments to `function' and `jacobian' aree same: x and y_i as scalars.

Both `function' and `jacobian' return multiple values.  `function'
returns `dimension' f_i values and `jacobian' returns
`dimension^2+dimension' values.

`jacobian' returns derivatives of `function' with respect to `x' and y_i.
The ordering of the `jacobian' values is as follows (the matrix view
is for convenience only, the values argument is a long list of
values):

 (values `d f_1 / d x'   `d f_2 / d x'   ... `d f_N / d x'
         `d f_1 / d y_1' `d f_1 / d y_2' ... `d f_1 / d y_N'
         `d f_2 / d y_1' `d f_2 / d y_2' ... `d f_2 / d y_N'
                               ...
         `d f_N / d y_1' `d f_N / d y_2' ... `d f_N / d y_N')
|#

(defmfun name ((object ode-stepper))
  "gsl_odeiv_step_name"
  (((mpointer object) :pointer))
  :definition :method
  :c-return :string
  :documentation			; FDL
  "The name of the stepping function.")

(defmfun step-order (stepper)
  "gsl_odeiv_step_order"
  (((mpointer stepper) :pointer))
  :c-return :uint
  :documentation			; FDL
  "The order of the stepping function on the previous
  step, which can vary if the stepping function itself is adaptive.")

(defmfun apply-step
    (stepper time y step-size yerr dydt-in dydt-out)
  "gsl_odeiv_step_apply"
  (((mpointer stepper) :pointer)
   (time :double)
   (step-size :double)
   (y :pointer)
   (yerr :pointer)
   (dydt-in :pointer)
   (dydt-out :pointer)
   ((callback-struct stepper) :pointer))
  :documentation			; FDL
  "Apply the stepping function stepper to the system of equations
   defined by make-ode-stepper, using the step size step-size to
   advance the system from time time and state y to time t +
   step-size.  The new state of the system is stored in y on output,
   with an estimate of the absolute error in each component stored in
   yerr If the argument dydt-in is not null it should point an array
   containing the derivatives for the system at time t on input. This
   is optional as the derivatives will be computed internally if they
   are not provided, but allows the reuse of existing derivative
   information.  On output the new derivatives of the system at time
   t + step-size will be stored in dydt-out if it is not null.

   User-supplied functions defined in the system dydt
   should signal an error or return the correct value.")

(defmpar +step-rk2+ "gsl_odeiv_step_rk2"
  ;; FDL
  "Embedded Runge-Kutta (2, 3) method.")

(defmpar +step-rk4+ "gsl_odeiv_step_rk4"
  ;; FDL
  "4th order (classical) Runge-Kutta.")

(defmpar +step-rkf45+ "gsl_odeiv_step_rkf45"
  ;; FDL
  "Embedded Runge-Kutta-Fehlberg (4, 5) method.  This method is a good
   general-purpose integrator.")

(defmpar +step-rkck+ "gsl_odeiv_step_rkck"
  ;; FDL
  "Embedded Runge-Kutta Cash-Karp (4, 5) method.")

(defmpar +step-rk8pd+ "gsl_odeiv_step_rk8pd"
  ;; FDL
  "Embedded Runge-Kutta Prince-Dormand (8,9) method.")

(defmpar +step-rk2imp+ "gsl_odeiv_step_rk2imp"
  ;; FDL
  "Implicit 2nd order Runge-Kutta at Gaussian points.")

(defmpar +step-rk4imp+ "gsl_odeiv_step_rk4imp"
  ;; FDL
  "Implicit 4th order Runge-Kutta at Gaussian points.")

(defmpar +step-bsimp+ "gsl_odeiv_step_bsimp"
  ;; FDL
  "Implicit Bulirsch-Stoer method of Bader and Deuflhard.  This algorithm
   requires the Jacobian.")

(defmpar +step-gear1+ "gsl_odeiv_step_gear1"
  ;; FDL
  "M=1 implicit Gear method.")

(defmpar +step-gear2+ "gsl_odeiv_step_gear2"
  ;; FDL
  "M=2 implicit Gear method.")
