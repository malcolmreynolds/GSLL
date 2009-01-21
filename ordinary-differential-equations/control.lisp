;; Adaptive step-size control
;; Liam Healy 2008-02-17 17:30:04EST control.lisp
;; Time-stamp: <2009-01-20 19:39:08EST control.lisp>
;; $Id$

(in-package :gsl)

(defmfun new-standard-control (absolute-error relative-error y-scaling dydt-scaling)
  "gsl_odeiv_control_standard_new"
  ((absolute-error :double) (relative-error :double)
   (y-scaling :double) (dydt-scaling :double))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation			; FDL
  "The standard control object is a four parameter heuristic based on
   absolute and relative errors absolute-error and relative-error, and
   scaling factors y-scaling and dydt-scaling for the system state y(t) and derivatives
   y'(t) respectively.

   The step-size adjustment procedure for this method begins by computing
   the desired error level D_i for each component,
   D_i = epsilon_{absolute} + epsilon_{relative} * (a_{y} |y_i| + a_{dydt} h |y'_i|)
   and comparing it with the observed error E_i = |yerr_i|.  If the
   observed error E exceeds the desired error level D by more
   than 10% for any component then the method reduces the step-size by an
   appropriate factor,
   h_{new} = h_{old} * S * (E/D)^{-1/q}
   where g is the consistency order of the method (e.g. q=4 for
   4(5) embedded RK), and S is a safety factor of 0.9. The ratio
   E/D is taken to be the maximum of the ratios E_i/D_i. 

   If the observed error E is less than 50% of the desired error
   level D for the maximum ratio E_i/D_i then the algorithm
   takes the opportunity to increase the step-size to bring the error in
   line with the desired level,
   h_{new} = h_{old} * S * (E/D)^{-1/(q+1)}
   This encompasses all the standard error scaling methods. To avoid
   uncontrolled changes in the stepsize, the overall scaling factor is
   limited to the range 1/5 to 5.")

(defmfun new-y-control (absolute-error relative-error)
  "gsl_odeiv_control_y_new"
  ((absolute-error :double) (relative-error :double))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation			; FDL
  "Create a new control object which will keep the local
   error on each step within an absolute error of absolute-error and
   relative error of relative-error with respect to the solution y_i(t).
   This is equivalent to the standard control object with y-scaling=1 and
   dydt-scaling=0.")

(defmfun new-yp-control (absolute-error relative-error)
  "gsl_odeiv_control_yp_new"
  ((absolute-error :double) (relative-error :double))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation			; FDL
  "Create a new control object which will keep the local
   error on each step within an absolute error of absolute-error and
   relative error of relative-error with respect to the derivatives of the
   solution y'_i(t).  This is equivalent to the standard control
   object with y-scaling=0 and dydt-scaling=1.")

(defmfun new-scaled-control
    (absolute-error relative-error y-scaling dydt-scaling absolute-scale dimension)
  "gsl_odeiv_control_scaled_new"
  ((absolute-error :double) (relative-error :double)
   (y-scaling :double) (dydt-scaling :double)
   (absolute-scale :pointer) (dimension sizet))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation			; FDL
  "Create a new control object which uses the same algorithm
   as #'new-standard-control but with an absolute error
   which is scaled for each component by the array absolute-error.
   The formula for D_i for this control object is
   D_i = epsilon_{abs} s_i + epsilon_{rel} * (a_{y} |y_i| + a_{dydt} h |y'_i|)
   where s_i is the i-th component of the array absolute-scale.
   The same error control heuristic is used by the Matlab ode suite.")

(defmfun control-alloc (control-type)
  "gsl_odeiv_control_alloc"
  ((control-type :pointer))
  :c-return (ptr :pointer)
  :return (ptr)
  :documentation			; FDL
  "Return a pointer to a newly allocated instance of a
   control function of type control-type.  This function is only needed for
   defining new types of control functions.  For most purposes the standard
   control functions described above should be sufficient.")

(defmfun initialize-control
    (control absolute-error relative-error y-scaling dydt-scaling)
  "gsl_odeiv_control_init"
  ((control :pointer) (absolute-error :double) (relative-error :double)
   (y-scaling :double) (dydt-scaling :double))
  :documentation			; FDL
  "Initialize the control function control with the
   parameters absolute-error, relative-error,
   y-scaling (scaling factor for y) and dydt-scaling (scaling 
   factor for derivatives).")

(defmfun free-control (control)
  "gsl_odeiv_control_free"
  ((control :pointer))
  :c-return :void
  :documentation			; FDL
  "Free all the memory associated with the control function control.")

(cffi:defcenum step-size-adjustment
  (:step-size-decreased -1) :step-size-unchanged :step-size-increased)

(defmfun adjust-stepsize (control stepper current-y y-error dydt step-size)
  "gsl_odeiv_control_hadjust"
  ((control :pointer) (stepper :pointer) (current-y :pointer)
   (y-error :pointer) (dydt :pointer) (step-size :pointer))
  :c-return :enumerate
  :enumeration step-size-adjustment
  :documentation			; FDL
  "Adjust the step-size using the control function
   and the current values of current-y, y-error and dydt.
   The stepping function stepper is also needed to determine the order
   of the method.  If the error in the y-values y-error is found to be
   too large then the step-size is reduced and the function returns
   :step-size-decreased.  If the error is sufficiently small then
   step-size may be increased and :step-size-increased is returned.  The
   function returns :step-size-unchanged if the step-size is
   unchanged.  The goal of the function is to estimate the largest
   step-size which satisfies the user-specified accuracy requirements for
   the current point.")

(defmfun control-name (control)
  "gsl_odeiv_control_name"
  ((control :pointer))
  :c-return :string
  :documentation			; FDL
  "The name of the control function.")
