;********************************************************
; file:        types.lisp                                
; description: Interpolation types                       
; date:        Sun Nov  4 2007 - 17:41                   
; author:      Liam Healy                                
; modified:    Sun Nov  4 2007 - 19:02
;********************************************************
;;; $Id: $

(in-package :gsl)

(defvariable *linear-interpolation* "gsl_interp_linear"
  "Linear interpolation.  This interpolation method does not require any
   additional memory.")

(defvariable *polynomial-interpolation* "gsl_interp_polynomial"
  "Polynomial interpolation.  This method should only be used for
   interpolating small numbers of points because polynomial interpolation
   introduces large oscillations, even for well-behaved datasets.  The
   number of terms in the interpolating polynomial is equal to the number
   of points.")

(defvariable *cubic-spline-interpolation* "gsl_interp_cspline"
  "Cubic spline with natural boundary conditions.  The resulting curve is
   piecewise cubic on each interval, with matching first and second
   derivatives at the supplied data-points.  The second derivative is
   chosen to be zero at the first point and last point.")

(defvariable *periodic-cubic-spline-interpolation* "gsl_interp_cspline_periodic"
  "Cubic spline with periodic boundary conditions.  The resulting curve
   is piecewise cubic on each interval, with matching first and second
   derivatives at the supplied data-points.  The derivatives at the first
   and last points are also matched.  Note that the last point in the
   data must have the same y-value as the first point, otherwise the
   resulting periodic interpolation will have a discontinuity at the
   boundary.")

(defvariable *akima-interpolation* "gsl_interp_akima"
  "Non-rounded Akima spline with natural boundary conditions.  This method
   uses the non-rounded corner algorithm of Wodicka.")

(defvariable *periodic-akima-interpolation* "gsl_interp_akima_periodic"
  "Non-rounded Akima spline with periodic boundary conditions.  This method
   uses the non-rounded corner algorithm of Wodicka.")

(defun-gsl interpolation-name (interpolation)
  "gsl_interp_name"
  ((interpolation :pointer))
  :c-return :string
  :documentation
  "This function returns the name of the interpolation type.")

(defun-gsl interpolation-minimum-size (interpolation)
  "gsl_interp_min_size"
  ((interpolation :pointer))
  :c-return :uint
  :documentation
  "The minimum number of points required by the
   interpolation.  For example, Akima spline interpolation
   requires a minimum of 5 points.")

(defun-gsl spline-name (interpolation)
  "gsl_spline_name"
  ((interpolation :pointer))
  :c-return :string
  :documentation
  "This function returns the name of the interpolation type.")

(defun-gsl spline-minimum-size (interpolation)
  "gsl_spline_min_size"
  ((interpolation :pointer))
  :c-return :uint
  :documentation
  "The minimum number of points required by the
   interpolation.  For example, Akima spline interpolation
   requires a minimum of 5 points.")
