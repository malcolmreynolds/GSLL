;; Example ODE                               
;; Liam Healy Sat Sep 29 2007 - 17:49
;; Time-stamp: <2009-02-15 08:39:44EST ode-example.lisp>
;; $Id$

;;; van der Pol as given in Section 25.5 of the GSL manual.  To
;;; reproduce that example, (integrate-vanderpol 100.0d0)

;;; Notice how the variable mu, which is a "parameter" in GSL's view,
;;; to be passed on to the evaluation functions, is declared special
;;; so its value is shared dynamically.  Thus GSL's parameters are
;;; always empty (and not accessible through user functions in GSLL).

(in-package :gsl)

(defun vanderpol (time y0 y1)
  (declare (special mu) (ignorable time))
  (values y1 (- (- y0) (* mu y1 (- (* y0 y0) 1)))))

(defun vanderpol-jacobian (time y0 y1)
  (declare (special mu) (ignorable time))
  (values 0.0d0 0.0d0			; dfdt
	  0.0d0 1.0d0			; dfdy in row-major order: 00, 01, 10, 11
	  (- (* -2 mu y0 y1) 1.0d0)
	  (* -1 mu (- (* y0 y0) 1.0d0))))

(defparameter *max-iter* 2000)

(make-callbacks ode-stepper vanderpol vanderpol-jacobian 2)

(defun integrate-vanderpol
    (max-time &optional (step-size 1.0d-6) (stepper *step-rk8pd*) (print-steps t))
  "Integrate the van der Pol oscillator as given in Section 25.5 of the
   GSL manual.  To reproduce that example, (integrate-vanderpol 100.0d0)."
  (let ((mu 10.0d0) (initial-time 0.0d0) (iter 0))
    (declare (special mu))
    (with-ode-integration
	((vanderpol vanderpol-jacobian)
	 time step max-time (dep0 dep1) 2 stepper)
      (setf dep0 1.0d0 dep1 0.0d0 step step-size time initial-time)
      (loop
	 (when (or (>= time max-time) (> iter *max-iter*))
	   (return (values iter time dep0 dep1)))
	 make-next-step
	 (incf iter)
	 (when print-steps
	   (format t "~12,6f~10t~12,6f~24t~12,6f~&" time dep0 dep1))))))

(save-test
 ode
 (integrate-vanderpol 1.0d0 1.d-4 *step-rk2* nil)
 (integrate-vanderpol 1.0d0 1.d-4 *step-rk4* nil)
 (integrate-vanderpol 1.0d0 1.d-4 *step-rkf45* nil)
 (integrate-vanderpol 1.0d0 1.d-4 *step-rkck* nil)
 (integrate-vanderpol 1.0d0 1.d-4 *step-rk8pd* nil)
 (integrate-vanderpol 1.0d0 1.d-4 *step-rk2imp* nil)
 (integrate-vanderpol 1.0d0 1.d-4 *step-rk4imp* nil)
 (integrate-vanderpol 1.0d0 1.d-4 *step-bsimp* nil)
 (let ((*max-iter* 12000))
   (integrate-vanderpol 1.0d0 1.d-4 *step-gear1* nil))
 (integrate-vanderpol 1.0d0 1.d-4 *step-gear2* nil))
