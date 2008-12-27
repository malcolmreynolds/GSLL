;; Interpolation allocation, initialization, and freeing.
;; Liam Healy, Sun Nov  4 2007 - 17:24
;; Time-stamp: <2008-12-26 18:19:19EST interpolation.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_interp.h
;;; /usr/include/gsl/gsl_spline.h

;;; A spline is an interpolation that also stores the arrays xa and ya,
;;; so they need not be supplied on each call.
(defmobject interpolation "gsl_interp"
  ((type :pointer) (size sizet))
  "interpolation"			; FDL
  "Make an interpolation object of type for size data-points,
   and optionally initialize the interpolation object interp for the
  data (xa,ya) where xa and ya are vectors.  The interpolation object does not save
  the data arrays xa and ya and only stores the static state
  computed from the data.  The xa data array is always assumed to be
  strictly ordered; the behavior for other arrangements is not defined."
  "init"
  (((c-pointer xa) :pointer) ((c-pointer ya) :pointer) ((dim0 xa) sizet))
  (lambda (set)
  `((type &optional xa-or-size (ya nil ,set))
    (:type type :size (if ,set (dim0 ya) xa-or-size))
    (:xa xa-or-size :ya ya))))

(defmobject spline "gsl_spline"
  ((type :pointer) (size sizet))
  "spline"			; FDL
  "Make an interpolation object of type for size data-points."
  "init"
  (((c-pointer xa) :pointer) ((c-pointer ya) :pointer) ((dim0 xa) sizet))
  (lambda (set)
  `((type &optional xa-or-size (ya nil ,set))
    (:type type :size (if ,set (dim0 ya) xa-or-size))
    (:xa xa-or-size :ya ya))))
