;; Evaluation of interpolation functions.
;; Liam Healy, Sun Nov  4 2007 - 18:40

(in-package :gsl)

(defmfun evaluate-interpolation (interpolation xa ya x acceleration)
  "gsl_interp_eval"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (x :double)
   (acceleration :pointer))
  :c-return :double
  :documentation			; FDL
  "Find the interpolated value of y for a given
   point x, using the interpolation object interpolation, data arrays
   xa and ya and the accelerator acceleration.")

(defmfun derivative-evaluate-interpolation (interpolation xa ya x acceleration)
  "gsl_interp_eval_deriv"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (x :double)
   (acceleration :pointer))
  :c-return :double
  :documentation			; FDL
  "Find the derivative of an interpolated function for a given point
   x, using the interpolation object interpolation, data arrays
   xa and ya and the accelerator acceleration.")

(defmfun second-derivative-evaluate-interpolation (interpolation xa ya x acceleration)
  "gsl_interp_eval_deriv2"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (x :double)
   (acceleration :pointer))
  :c-return :double
  :documentation			; FDL
  "Find the second derivative of an interpolated function for a given point
   x, using the interpolation object interpolation, data arrays
   xa and ya and the accelerator acceleration.")

(defmfun integral-evaluate-interpolation (interpolation xa ya x low high acceleration)
  "gsl_interp_eval_integ"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (x :double)
   (low :double) (high :double) (acceleration :pointer))
  :c-return :double
  :documentation			; FDL
  "Find the numerical integral of an interpolated function over the
   range [low, high], using the interpolation object interpolation,
   data arrays xa and ya and the accelerator 'acceleration.")

;;; Spline
(defmfun evaluate-spline (spline x acceleration)
  "gsl_spline_eval"
  ((spline :pointer) (x :double) (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the interpolated value of y for a given
   point x, using the spline object spline, data arrays
   xa and ya and the accelerator 'acceleration.")

(defmfun derivative-evaluate-spline (spline x acceleration)
  "gsl_spline_eval_deriv"
  ((spline :pointer) (x :double) (acceleration :pointer))
  :c-return :double
  :documentation			; FDL
  "Find the derivative of an interpolated function for a given point
   x, using the spline object spline, data arrays
   xa and ya and the accelerator acceleration.")

(defmfun second-derivative-evaluate-spline (spline x acceleration)
  "gsl_spline_eval_deriv2"
  ((spline :pointer) (x :double) (acceleration :pointer))
  :c-return :double
  :documentation			; FDL
  "Find the second derivative of an interpolated function for a given point
   x, using the spline object spline, data arrays
   xa and ya and the accelerator acceleration.")

(defmfun integral-evaluate-spline (spline x low high acceleration)
  "gsl_spline_eval_integ"
  ((spline :pointer) (x :double) (low :double) (high :double)
   (acceleration :pointer))
  :c-return :double
  :documentation			; FDL
  "Find the numerical integral of an interpolated function over the
   range [low, high], using the spline object spline,
   data arrays xa and ya and the accelerator acceleration.")
