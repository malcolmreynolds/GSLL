;; Interpolation allocation, initialization, and freeing.
;; Liam Healy, Sun Nov  4 2007 - 17:24
;; Time-stamp: <2008-02-17 17:45:18EST interpolation.lisp>
;; $Id$

(in-package :gsl)

;;; A spline is an interpolation that also stores the arrays xa and ya,
;;; so they need not be supplied on each call.

(defgo interpolation (type xa-or-size &optional ya)
  (list
   `(allocate-interpolation ,type ,(if ya `(dim0 ,ya) xa-or-size))
   'free-spline
   (when ya
     (lambda (symb)
       `(initialize-interpolation ,symb ,xa-or-size ,ya)))))

;;; Interpolation
(defmfun allocate-interpolation (type size)
  "gsl_interp_alloc"
  ((type :pointer) (size size))
  :c-return :pointer
  :export nil
  :index (letm interpolation)
  :documentation			; FDL
  "Allocate an interpolation object of type for size data-points,
   and return the pointer to it.")

(defmfun initialize-interpolation (interpolation xa ya)
  "gsl_interp_init"
  ((interpolation :pointer)
   ((gsl-array xa) :pointer) ((gsl-array ya) :pointer) ((dim0 xa) size))
  :documentation			; FDL
  "Initialize the interpolation object interp for the
  data (xa,ya) where xa and ya are gsl-arrays.  The interpolation object does not save
  the data arrays xa and ya and only stores the static state
  computed from the data.  The xa data array is always assumed to be
  strictly ordered; the behavior for other arrangements is not defined.")

(defmfun free-interpolation (interpolation)
  "gsl_interp_free"
  ((interpolation :pointer))
  :c-return :void
  :export nil
  :index (letm interpolation)
  :documentation			; FDL
  "Frees the interpolation object interp.")

(defgo spline (type xa-or-size &optional ya)
  (list
   `(allocate-spline ,type ,(if ya `(dim0 ,ya) xa-or-size))
   'free-spline
   (when ya
     (lambda (symb)
       `(initialize-spline ,symb ,xa-or-size ,ya)))))

;;; Spline
(defmfun allocate-spline (type size)
  "gsl_spline_alloc"
  ((type :pointer) (size size))
  :c-return :pointer
  :documentation			; FDL
  "Allocate an interpolation object of type for size data-points,
   and return the pointer to it.")

(defmfun initialize-spline (interpolation xa ya)
  "gsl_spline_init"
  ((interpolation :pointer)
   ((gsl-array xa) :pointer) ((gsl-array ya) :pointer) ((dim0 xa) size))
  :documentation			; FDL
  "Initialize the interpolation object interp for the
  data (xa,ya) where xa and ya are gsl-arrays.  The spline object saves
  the data arrays xa and ya computed from the data.
  The xa data array is always assumed to be
  strictly ordered; the behavior for other arrangements is not defined.")

(defmfun free-spline (interpolation)
  "gsl_spline_free"
  ((interpolation :pointer))
  :c-return :void
  :documentation			; FDL
  "Frees the spline object.")
