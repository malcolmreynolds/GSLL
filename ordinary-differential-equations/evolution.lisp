;; Evolution functions for ODE integration.
;; Liam Healy, Sun Sep 30 2007 - 14:31
;; Time-stamp: <2008-02-17 17:43:24EST evolution.lisp>
;; $Id: $

(in-package :gsl)

(defmfun allocate-evolution (dimension)
  "gsl_odeiv_evolve_alloc"
  ((dimension size))
  :c-return :pointer
  :documentation			; FDL
  "Allocate a new instance of an evolution function
   for a system of dimension dimensions and return the pointer.")

(defmfun apply-evolution
    (evolve control step dydt time max-time step-size y)
  "gsl_odeiv_evolve_apply"
  ((evolve :pointer) (control :pointer) (step :pointer)
   (dydt :pointer) (time :pointer) (max-time :double)
   (step-size :pointer) (y :pointer))
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

(defmfun reset-evolution (evolve)
  "gsl_odeiv_evolve_reset"
  ((evolve :pointer))
  :documentation
  "Reset the evolution function evolve.  It should be used
   whenever the next use of evolve will not be a continuation of a
   previous step.")

(defmfun free-evolution (evolve)
    "gsl_odeiv_evolve_free"
  ((evolve :pointer))
  :c-return :void
  :documentation
  "Frees all the memory associated with the evolution function.")
