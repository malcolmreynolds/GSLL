;********************************************************
; file:        control.lisp                              
; description: Adaptive step-size control                
; date:        Sat Sep 29 2007 - 18:51                   
; author:      Liam Healy                                
; modified:    Sat Sep 29 2007 - 22:51
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl new-standard-control (absolute-error relative-error y dydt)
  "gsl_odeiv_control_standard_new"
  ((absolute-error :double) (relative-error :double) (y :double) (dydt :double))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation
  "The standard control object is a four parameter heuristic based on
   absolute and relative errors @var{eps_absolute} and @var{eps_relative}, and
   scaling factors @var{a_y} and @var{a_dydt} for the system state
   @math{y(t)} and derivatives @math{y'(t)} respectively.

   The step-size adjustment procedure for this method begins by computing
   the desired error level @math{D_i} for each component,
   D_i = \epsilon_{absolute} + \epsilon_{relative} * (a_{y} |y_i| + a_{dydt} h |y'_i|)
   and comparing it with the observed error @math{E_i = |yerr_i|}.  If the
   observed error @var{E} exceeds the desired error level @var{D} by more
   than 10% for any component then the method reduces the step-size by an
   appropriate factor,
   h_{new} = h_{old} * S * (E/D)^{-1/q}
   where @math{q} is the consistency order of the method (e.g. @math{q=4} for
   4(5) embedded RK), and @math{S} is a safety factor of 0.9. The ratio
   @math{E/D} is taken to be the maximum of the ratios @math{E_i/D_i}. 

   If the observed error @math{E} is less than 50% of the desired error
   level @var{D} for the maximum ratio @math{E_i/D_i} then the algorithm
   takes the opportunity to increase the step-size to bring the error in
   line with the desired level,
   h_{new} = h_{old} * S * (E/D)^{-1/(q+1)}
   This encompasses all the standard error scaling methods. To avoid
   uncontrolled changes in the stepsize, the overall scaling factor is
   limited to the range @math{1/5} to 5.")

(defun-gsl new-y-control (absolute-error relative-error)
  "gsl_odeiv_control_y_new"
  ((absolute-error :double) (relative-error :double))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation
  "Create a new control object which will keep the local
   error on each step within an absolute error of @var{eps_absolute} and
   relative error of @var{eps_relative} with respect to the solution @math{y_i(t)}.
   This is equivalent to the standard control object with @var{a_y}=1 and
   @var{a_dydt}=0.")

(defun-gsl new-yp-control (absolute-error relative-error)
  "gsl_odeiv_control_yp_new"
  ((absolute-error :double) (relative-error :double))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation
  "Create a new control object which will keep the local
   error on each step within an absolute error of @var{eps_absolute} and
   relative error of @var{eps_relative} with respect to the derivatives of the
   solution @math{y'_i(t)}.  This is equivalent to the standard control
   object with @var{a_y}=0 and @var{a_dydt}=1.")

(defun-gsl new-scaled-control
    (absolute-error relative-error y dydt absolute-scale dimension)
  "gsl_odeiv_control_scaled_new"
  ((absolute-error :double) (relative-error :double) (y :double) (dydt :double)
   (absolute-scale :pointer) (dimension :size))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation
  "Create a new control object which uses the same algorithm
   as @code{gsl_odeiv_control_standard_new} but with an absolute error
   which is scaled for each component by the array @var{scale_abs}.
   The formula for @math{D_i} for this control object is
   D_i = \epsilon_{abs} s_i + \epsilon_{rel} * (a_{y} |y_i| + a_{dydt} h |y'_i|)
   where @math{s_i} is the @math{i}-th component of the array @var{scale_abs}.
   The same error control heuristic is used by the Matlab @sc{ode} suite.")

(defun-gsl control-alloc (control-type)
  "gsl_odeiv_control_alloc"
  ((control-type :pointer))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation
  "Return a pointer to a newly allocated instance of a
   control function of type @var{T}.  This function is only needed for
   defining new types of control functions.  For most purposes the standard
   control functions described above should be sufficient.")

(defun-gsl initialize-control
    (control absolute-error relative-error y-scaling dydt-scaling)
  "gsl_odeiv_control_init"
  ((control :pointer) (absolute-error :double) (relative-error :double)
   (y-scaling :double) (dydt-scaling :double))
  :documentation
  "Initialize the control function @var{control} with the
   parameters absolute-error, relative-error,
   y-scaling (scaling factor for y) and dydt-scaling (scaling 
   factor for derivatives).")

(defun-gsl free-control (control)
  "gsl_odeiv_control_free"
  ((control :pointer))
  :c-return :void
  :documentation
  "Free all the memory associated with the control function
   @var{control}.")

(cffi:defcenum step-size-adjustment
  (:step-size-decreased -1) :step-size-unchanged :step-size-increased)

(defun-gsl adjust-stepsize (control stepper current-y y-error dydt step-size)
  "gsl_odeiv_control_hadjust"
  ((control :pointer) (stepper :pointer) (current-y :pointer)
   (y-error :pointer) (dydt :pointer) (step-size :pointer))
  :c-return :enumerate
  :enumeration step-size-adjustment
  :documentation
  "Adjust the step-size @var{h} using the control function
   and the current values of current-y, y-error and @var{dydt}.
   The stepping function stepper is also needed to determine the order
   of the method.  If the error in the y-values y-error is found to be
   too large then the step-size is reduced and the function returns
   :step-size-decreased.  If the error is sufficiently small then
   step-size may be increased and :step-size-increased is returned.  The
   function returns :step-size-unchanged if the step-size is
   unchanged.  The goal of the function is to estimate the largest
   step-size which satisfies the user-specified accuracy requirements for
   the current point.")

(defun-gsl control-name (control)
  "gsl_odeiv_control_name"
  ((control :pointer))
  :c-return :string
  :documentation "The name of the control function.")
