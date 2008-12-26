;; Interpolation types
;; Liam Healy, Sun Nov  4 2007 - 17:41
;; Time-stamp: <2008-12-26 17:23:12EST types.lisp>
;; $Id$

(in-package :gsl)

(defmpar *linear-interpolation* "gsl_interp_linear"
  ;; FDL
  "Linear interpolation.  This interpolation method does not require any
   additional memory.")

(defmpar *polynomial-interpolation* "gsl_interp_polynomial"
  ;; FDL
  "Polynomial interpolation.  This method should only be used for
   interpolating small numbers of points because polynomial interpolation
   introduces large oscillations, even for well-behaved datasets.  The
   number of terms in the interpolating polynomial is equal to the number
   of points.")

(defmpar *cubic-spline-interpolation* "gsl_interp_cspline"
  ;; FDL
  "Cubic spline with natural boundary conditions.  The resulting curve is
   piecewise cubic on each interval, with matching first and second
   derivatives at the supplied data-points.  The second derivative is
   chosen to be zero at the first point and last point.")

(defmpar *periodic-cubic-spline-interpolation* "gsl_interp_cspline_periodic"
  ;; FDL
  "Cubic spline with periodic boundary conditions.  The resulting curve
   is piecewise cubic on each interval, with matching first and second
   derivatives at the supplied data-points.  The derivatives at the first
   and last points are also matched.  Note that the last point in the
   data must have the same y-value as the first point, otherwise the
   resulting periodic interpolation will have a discontinuity at the
   boundary.")

(defmpar *akima-interpolation* "gsl_interp_akima"
  ;; FDL
  "Non-rounded Akima spline with natural boundary conditions.  This method
   uses the non-rounded corner algorithm of Wodicka.")

(defmpar *periodic-akima-interpolation* "gsl_interp_akima_periodic"
  ;; FDL
  "Non-rounded Akima spline with periodic boundary conditions.  This method
   uses the non-rounded corner algorithm of Wodicka.")

(defmfun name ((interpolation interpolation))
  "gsl_interp_name"
  ((interpolation :pointer))
  :definition :method
  :c-return :string
  :documentation			; FDL
  "The name of the interpolation type.")

(defmfun interpolation-minimum-size (interpolation)
  "gsl_interp_min_size"
  ((interpolation :pointer))
  :c-return :uint
  :documentation			; FDL
  "The minimum number of points required by the
   interpolation.  For example, Akima spline interpolation
   requires a minimum of 5 points.")

(defmfun name ((interpolation spline))
  "gsl_spline_name"
  ((interpolation :pointer))
  :definition :method
  :c-return :string
  :documentation			; FDL
  "The name of the interpolation type.")

(defmfun spline-minimum-size (interpolation)
  "gsl_spline_min_size"
  ((interpolation :pointer))
  :c-return :uint
  :documentation			; FDL
  "The minimum number of points required by the
   interpolation.  For example, Akima spline interpolation
   requires a minimum of 5 points.")
