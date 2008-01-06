;********************************************************
; file:        ode-example.lisp                          
; description: Example ODE                               
; date:        Sat Sep 29 2007 - 17:49                   
; author:      Liam Healy                                
; modified:    Sat Jan  5 2008 - 21:41
;********************************************************
;;; $Id: $

;;; van der Pol as given in Section 25.5 of the GSL manual.  To
;;; reproduce that example, (integrate-vanderpol 100.0d0)

(in-package :gsl)

(defun vanderpol (time y dydt)
  (declare (special mu) (ignorable time))
  (with-c-doubles ((y y0 y1) (dydt dydt0 dydt1))
    (setf dydt0 y1
	  dydt1 (- (- y0) (* mu y1 (- (* y0 y0) 1))))))

(defun vanderpol-jacobian (time y dfdy dfdt)
  (declare (special mu) (ignorable time))
  (with-c-doubles ((y y0 y1) (dfdy dfdy0 dfdy1 dfdy2 dfdy3) (dfdt dfdt0 dfdt1))
    (setf dfdt0 0.0d0
	  dfdt1 0.0d0
	  dfdy0 0.0d0
	  dfdy1 1.0d0
	  dfdy2 (- (* -2 mu y0 y1) 1.0d0)
	  dfdy3 (* -1 mu (- (* y0 y0) 1.0d0)))))

(def-ode-functions vanderpol vanderpol-jacobian 2)

(defparameter *max-iter* 2000)

(defun integrate-vanderpol (max-time &optional (step-size 1.0d-6))
  "Integrate the van der Pol oscillator as given in Section 25.5 of the
   GSL manual.  To reproduce that example, (integrate-vanderpol 100.0d0)."
  (let ((mu 10.0d0) (time 0.0d0) (iter 0))
    (declare (special mu))
    (with-ode-integration (time step-size (dependent dep0 dep1) 2)
      (setf dep0 1.0d0 dep1 0.0d0)
      (loop (when (or (>= (double-to-cl time) max-time) (> iter *max-iter*)) (return))
	    (apply-evolution
	     evolve control stepper vanderpol
	     time max-time step-size dependent)
	    (incf iter)
	    (format t "~&~12,6f~10t~12,6f~24t~12,6f"
		    (double-to-cl time) dep0 dep1)))))

