;; Evaluation of interpolation functions.
;; Liam Healy Sun Nov  4 2007 - 18:40
;; Time-stamp: <2009-01-28 19:22:15EST evaluation.lisp>

(in-package :gsl)

(defmfun evaluate ((interpolation interpolation) x &key xa ya acceleration)
  "gsl_interp_eval"
  (((mpointer interpolation) :pointer) (xa :pointer) (ya :pointer) (x :double)
   ((mpointer acceleration) :pointer))
  :definition :method
  :inputs (xa ya)
  :c-return :double
  :documentation			; FDL
  "Find the interpolated value of y for a given
   point x, using the interpolation object interpolation, data arrays
   xa and ya and the accelerator acceleration.")

(export 'evaluate-derivative)
(defgeneric evaluate-derivative (object point &key)
  (:documentation			; FDL
   "Find the derivative of an interpolated function for a given point
   x, using the interpolation object interpolation, data arrays
   xa and ya and the accelerator acceleration."))

(defmfun evaluate-derivative
    ((interpolation interpolation) x &key xa ya acceleration)
  "gsl_interp_eval_deriv"
  (((mpointer interpolation) :pointer) (xa :pointer) (ya :pointer) (x :double)
   ((mpointer acceleration) :pointer))
  :definition :method
  :inputs (xa ya)
  :c-return :double)

(export 'evaluate-second-derivative)
(defgeneric evaluate-second-derivative (object point &key)
  (:documentation			; FDL
   "Find the second derivative of an interpolated function for a given point
   x, using the interpolation object interpolation, data arrays
   xa and ya and the accelerator acceleration."))

(defmfun evaluate-second-derivative
    ((interpolation interpolation) x &key xa ya acceleration)
  "gsl_interp_eval_deriv2"
  (((mpointer interpolation) :pointer) (xa :pointer) (ya :pointer) (x :double)
   ((mpointer acceleration) :pointer))
  :definition :method
  :inputs (xa ya)
  :c-return :double)

(export 'evaluate-integral)
(defgeneric evaluate-integral (object lower-limit upper-limit &key)
  (:documentation			; FDL
   "Find the numerical integral of an interpolated function over the
   range [low, high], using the interpolation object interpolation,
   data arrays xa and ya and the accelerator 'acceleration."))

(defmfun evaluate-integral
    ((interpolation interpolation) low high &key xa ya acceleration)
  "gsl_interp_eval_integ"
  (((mpointer interpolation) :pointer) (xa :pointer) (ya :pointer)
   (low :double) (high :double) ((mpointer acceleration) :pointer))
  :definition :method
  :inputs (xa ya)
  :c-return :double)

;;; Spline
(defmfun evaluate ((spline spline) x &key acceleration)
  "gsl_spline_eval"
  (((mpointer spline) :pointer) (x :double) ((mpointer acceleration) :pointer))
  :definition :method
  :c-return :double)

(defmfun evaluate-derivative ((spline spline) x &key acceleration)
  "gsl_spline_eval_deriv"
  (((mpointer spline) :pointer) (x :double) ((mpointer acceleration) :pointer))
  :definition :method
  :c-return :double)

(defmfun evaluate-second-derivative ((spline spline) x &key acceleration)
  "gsl_spline_eval_deriv2"
  (((mpointer spline) :pointer) (x :double) ((mpointer acceleration) :pointer))
  :definition :method
  :c-return :double)

(defmfun evaluate-integral ((spline spline) low high &key acceleration)
  "gsl_spline_eval_integ"
  (((mpointer spline) :pointer) (low :double) (high :double)
   ((mpointer acceleration) :pointer))
  :definition :method
  :c-return :double)
