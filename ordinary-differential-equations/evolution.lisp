;********************************************************
; file:        evolution.lisp                            
; description: Evolution functions for ODE integration.  
; date:        Sun Sep 30 2007 - 14:31                   
; author:      Liam Healy                                
; modified:    Thu Oct 18 2007 - 22:18
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl allocate-evolution (dimension)
  "gsl_odeiv_evolve_alloc"
  ((dimension :size))
  :c-return :pointer
  :documentation
  "Allocate a new instance of an evolution function
   for a system of dimension dimensions and return the pointer.")

(defun-gsl apply-evolution
    (evolve control step dydt time max-time step-size y)
  "gsl_odeiv_evolve_apply"
  ((evolve :pointer) (control :pointer) (step :pointer)
   (dydt :pointer) (time :pointer) (max-time :double)
   (step-size :pointer) (y :pointer))
  :documentation
  "Advance the system (@var{e}, @var{dydt}) from time
   @var{t} and position @var{y} using the stepping function @var{step}.
   The new time and position are stored in @var{time} and @var{y} on output.
   The initial step-size is taken as @var{step-size}, but this will be modified
   using the control function @var{c} to achieve the appropriate error
   bound if necessary.  The routine may make several calls to @var{step} in
   order to determine the optimum step-size. If the step-size has been
   changed the value of @var{h} will be modified on output.  The maximum
   time @var{max-time} is guaranteed not to be exceeded by the time-step.  On the
   final time-step the value of @var{time} will be set to @var{t1} exactly.")

(defun-gsl reset-evolution (evolve)
  "gsl_odeiv_evolve_reset"
  ((evolve :pointer))
  :documentation
  "Reset the evolution function @var{evolve}.  It should be used
   whenever the next use of @var{evolve} will not be a continuation of a
   previous step.")

(defun-gsl free-evolution (evolve)
    "gsl_odeiv_evolve_free"
  ((evolve :pointer))
  :c-return :void
  :documentation
  "Frees all the memory associated with the evolution function.")
