;********************************************************
; file:        evaluation.lisp                           
; description: Evaluation of interpolation functions.    
; date:        Sun Nov  4 2007 - 18:40                   
; author:      Liam Healy                                
; modified:    Sun Nov  4 2007 - 19:04
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl evaluate-interpolation (interpolation xa ya x acceleration)
  "gsl_interp_eval"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (x :double)
   (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the interpolated value of @var{y} for a given
   point @var{x}, using the interpolation object 'interpolation, data arrays
   @var{xa} and @var{ya} and the accelerator 'acceleration.")

(defun-gsl derivative-evaluate-interpolation (interpolation xa ya x acceleration)
  "gsl_interp_eval_deriv"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (x :double)
   (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the derivative of an interpolated function for a given point
   @var{x}, using the interpolation object 'interpolation, data arrays
   @var{xa} and @var{ya} and the accelerator 'acceleration.")

(defun-gsl second-derivative-evaluate-interpolation (interpolation xa ya x acceleration)
  "gsl_interp_eval_deriv2"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (x :double)
   (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the second derivative of an interpolated function for a given point
   @var{x}, using the interpolation object 'interpolation, data arrays
   @var{xa} and @var{ya} and the accelerator 'acceleration.")

(defun-gsl integral-evaluate-interpolation (interpolation xa ya x low high acceleration)
  "gsl_interp_eval_integ"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (x :double)
   (low :double) (high :double) (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the numerical integral of an interpolated function over the
   range [low, high], using the interpolation object 'interpolation,
   data arrays @var{xa} and @var{ya} and the accelerator 'acceleration.")

;;; Spline
(defun-gsl evaluate-spline (spline x acceleration)
  "gsl_spline_eval"
  ((spline :pointer) (x :double) (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the interpolated value of @var{y} for a given
   point @var{x}, using the spline object 'spline, data arrays
   @var{xa} and @var{ya} and the accelerator 'acceleration.")

(defun-gsl derivative-evaluate-spline (spline x acceleration)
  "gsl_spline_eval_deriv"
  ((spline :pointer) (x :double) (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the derivative of an interpolated function for a given point
   @var{x}, using the spline object 'spline, data arrays
   @var{xa} and @var{ya} and the accelerator 'acceleration.")

(defun-gsl second-derivative-evaluate-spline (spline x acceleration)
  "gsl_spline_eval_deriv2"
  ((spline :pointer) (x :double) (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the second derivative of an interpolated function for a given point
   @var{x}, using the spline object 'spline, data arrays
   @var{xa} and @var{ya} and the accelerator 'acceleration.")

(defun-gsl integral-evaluate-spline (spline x low high acceleration)
  "gsl_spline_eval_integ"
  ((spline :pointer) (x :double) (low :double) (high :double)
   (acceleration :pointer))
  :c-return :double
  :documentation
  "Find the numerical integral of an interpolated function over the
   range [low, high], using the spline object 'spline,
   data arrays @var{xa} and @var{ya} and the accelerator 'acceleration.")
