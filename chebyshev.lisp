;; Chebyshev Approximations
;; Liam Healy Sat Nov 17 2007 - 20:36
;; Time-stamp: <2009-02-15 11:23:40EST chebyshev.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_chebyshev.h

;;;;****************************************************************************
;;;; Creation and calculation of Chebyshev series
;;;;****************************************************************************

(defmobject chebyshev "gsl_cheb"
  ((order sizet))
  "Chebyshev series"
  :documentation			; FDL
  "Make a Chebyshev series of specified order."
  :superclasses (callback-included)
  :ci-class-slots (gsl-function nil (function))
  :initialize-suffix "init"
  :initialize-args
  ((callback :pointer) (lower-limit :double) (upper-limit :double))
  :singular (function))

;;;;****************************************************************************
;;;; Chebyshev series evaluation
;;;;****************************************************************************

;;; The functions that don't return are defined, but it is recommended
;;; to use the functions that do return error (and ignore it if
;;; desired) in the form of #'evaluate.

(defmfun evaluate ((object chebyshev) x &key order)
  ("gsl_cheb_eval" "gsl_cheb_eval_n")
  ((((mpointer object) :pointer) (x :double))
   (((mpointer object) :pointer) (order sizet) (x :double)))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "Evaluate the Chebyshev series at a point x.  If order is supplied,
  evaluate to at most the given order.")

(defmfun evaluate-chebyshev-error (chebyshev x &optional order)
  ("gsl_cheb_eval_err" "gsl_cheb_eval_n_err")
  ((((mpointer chebyshev) :pointer) (x :double) (result :double)
    (abserr :double))
   (((mpointer chebyshev) :pointer) (order sizet) (x :double)
    (result :double) (abserr :double)))
  :documentation			; FDL
  "Evaluate the Chebyshev series at a point x, returning result and
   an estimate of its absolute error.  If order is supplied,
   evaluate to at most the given order.")

;;;;****************************************************************************
;;;; Derivatives and integrals
;;;;****************************************************************************

(defmfun derivative-chebyshev (derivative chebyshev)
  "gsl_cheb_calc_deriv"
  (((mpointer derivative) :pointer) ((mpointer chebyshev) :pointer))
  :documentation			; FDL
  "Compute the derivative of the Chebyshev series, storing
   the derivative coefficients in the previously allocated series.
   The two series must have been allocated with the same order.")

(defmfun integral-chebyshev (integral chebyshev)
  "gsl_cheb_calc_integ"
  (((mpointer integral) :pointer) ((mpointer chebyshev) :pointer))
  :documentation			; FDL
  "Compute the integral of the Chebyshev series, storing
   the integral coefficients in the previously allocated series.
   The two series must have been allocated with the same order.
   The lower limit of the integration is taken to be the left hand
   end of the range lower-limit.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************

;;; From Chap. 28.5, except I have set steps = 100 instead of 10000
;;; to keep things sane.

(defun chebyshev-step (x) (if (< x 0.5d0) 0.25d0 0.75d0))

(make-callbacks single-function chebyshev-step)

(defun chebyshev-table-example ()
  (let ((steps 100))
    (let ((cheb (make-chebyshev 40 'chebyshev-step 0.0d0 1.0d0)))
      (dotimes (i steps)
	(let ((x (coerce (/ i steps) 'double-float)))
	  (format t "~&~a ~a ~a ~a"
		  x
		  (chebyshev-step x)
		  (evaluate cheb x :order 10)
		  (evaluate cheb x)))))))

(defun chebyshev-point-example (x)
  (check-type x double-float)
  (let ((cheb (make-chebyshev 40 'chebyshev-step 0.0d0 1.0d0))
	(deriv (make-chebyshev 40))
	(integ (make-chebyshev 40)))
    (derivative-chebyshev deriv cheb)
    (integral-chebyshev integ cheb)
    (list
     (evaluate cheb x)
     (evaluate deriv x)
     (evaluate integ x))))

;;; Unit test
(save-test chebyshev
  (chebyshev-point-example 0.55d0))
