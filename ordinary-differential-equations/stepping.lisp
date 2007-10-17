;********************************************************
; file:        stepping.lisp                             
; description: Stepping functions for ODE systems        
; date:        Mon Sep 24 2007 - 21:33                   
; author:      Liam Healy                                
; modified:    Tue Oct 16 2007 - 22:45
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl step-allocate (step-type dim)
  "gsl_odeiv_step_alloc"
  ((step-type :pointer) (dim :size))
  :c-return :pointer
  :documentation
  "Allocate a new instance of a stepping function of
   type @var{T} for a system of @var{dim} dimensions,
   returning the pointer.")

(defun-gsl step-reset (stepper)
  "gsl_odeiv_step_reset"
  ((stepper :pointer))
  :documentation
  "Reset the stepping function @var{s}.  It should be used whenever
   the next use of @var{s} will not be a continuation of a previous
   step.")

(defun-gsl step-free (stepper)
  "gsl_odeiv_step_free"
  ((stepper :pointer))
  :c-return :void
  :documentation
  "Free all the memory associated with the stepping function.")

(defun-gsl step-name (stepper)
  "gsl_odeiv_step_name"
  ((stepper :pointer))
  :c-return :string
  :documentation "The name of the stepping function.")

(defun-gsl step-order (stepper)
  "gsl_odeiv_step_order"
  ((stepper :pointer))
  :c-return :uint
  :documentation "The order of the stepping function on the previous
  step, which can vary if the stepping function itself is adaptive.")

(defun-gsl step-apply
    (stepper time step-size y yerr dydt-in dydt-out dydt)
  "gsl_odeiv_step_apply"
  ((stepper :pointer)
   (time :double)
   (step-size :double)
   (y :pointer)
   (yerr :pointer)
   (dydt-in :pointer)
   (dydt-out :pointer)
   (dydt :pointer))
  :documentation
  "Apply the stepping function stepper to the system of
   equations defined by @var{dydt}, using the step size step-size to advance
   the system from time @var{t} and state @var{y} to time @var{t}+@var{h}.
   The new state of the system is stored in @var{y} on output, with an
   estimate of the absolute error in each component stored in @var{yerr}.
   If the argument @var{dydt_in} is not null it should point an array
   containing the derivatives for the system at time @var{t} on input. This
   is optional as the derivatives will be computed internally if they are
   not provided, but allows the reuse of existing derivative information.
   On output the new derivatives of the system at time @var{t}+@var{h} will
   be stored in @var{dydt_out} if it is not null.

   If the user-supplied functions defined in the system @var{dydt} return a
   status other than @code{GSL_SUCCESS} the step will be aborted.  In this
   case, the elements of @var{y} will be restored to their pre-step values
   and the error code from the user-supplied function will be returned.  To
   distinguish between error codes from the user-supplied functions and
   those from @code{gsl_odeiv_step_apply} itself, any user-defined return
   values should be distinct from the standard GSL error codes.")

(defmacro defstep (cl-symbol gsl-symbol documentation)
  `(progn
    (cffi:defcvar (,gsl-symbol ,cl-symbol) :pointer :read-only t)
    (setf (documentation ',cl-symbol 'variable) ,documentation)))

(defstep *step-rk2* "gsl_odeiv_step_rk2"
  "Embedded Runge-Kutta (2, 3) method.")

(defstep *step-rk4* "gsl_odeiv_step_rk4"
  "4th order (classical) Runge-Kutta.")

(defstep *step-rkf45* "gsl_odeiv_step_rkf45"
  "Embedded Runge-Kutta-Fehlberg (4, 5) method.  This method is a good
   general-purpose integrator.")

(defstep *step-rkck* "gsl_odeiv_step_rkck"
  "Embedded Runge-Kutta Cash-Karp (4, 5) method.")

(defstep *step-rk8pd* "gsl_odeiv_step_rk8pd"
  "Embedded Runge-Kutta Prince-Dormand (8,9) method.")

(defstep *step-rk2imp* "gsl_odeiv_step_rk2imp"
  "Implicit 2nd order Runge-Kutta at Gaussian points.")

(defstep *step-rk4imp* "gsl_odeiv_step_rk4imp"
  "Implicit 4th order Runge-Kutta at Gaussian points.")

(defstep *step-bsimp* "gsl_odeiv_step_bsimp"
  "Implicit Bulirsch-Stoer method of Bader and Deuflhard.  This algorithm
   requires the Jacobian.")

(defstep *step-gear1* "gsl_odeiv_step_gear1"
  "M=1 implicit Gear method.")

(defstep *step-gear2* "gsl_odeiv_step_gear2"
  "M=2 implicit Gear method.")
