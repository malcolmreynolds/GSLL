;********************************************************
; file:        transport.lisp                                
; description: Transport functions
; date:        Mon May  1 2006 - 22:29
; author:      Liam M. Healy                             
; modified:    Sat Jun 17 2006 - 22:33
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; The transport functions @math{J(n,x)} are defined by the integral 
;;; representations
;;; @c{$J(n,x) := \int_0^x dt \, t^n e^t /(e^t - 1)^2$}
;;; @math{J(n,x) := \int_0^x dt t^n e^t /(e^t - 1)^2}.

(defun-gsl transport-2 (x)
  "gsl_sf_transport_2_e" ((x :double) (ret sf-result))
  :documentation "The transport function @math{J(2,x)}.")

(defun-gsl transport-3 (x)
  "gsl_sf_transport_3_e" ((x :double) (ret sf-result))
  :documentation "The transport function @math{J(3,x)}.")

(defun-gsl transport-4 (x)
  "gsl_sf_transport_4_e" ((x :double) (ret sf-result))
  :documentation "The transport function @math{J(4,x)}.")

(defun-gsl transport-5 (x)
  "gsl_sf_transport_5_e" ((x :double) (ret sf-result))
  :documentation "The transport function @math{J(5,x)}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test transport
  (lisp-unit:assert-first-fp-equal "0.280666640456d+01" (transport-2 4.0d0))
  (lisp-unit:assert-first-fp-equal "0.457921743723d+01" (transport-3 4.0d0))
  (lisp-unit:assert-first-fp-equal "0.107319323930d+02" (transport-4 4.0d0))
  (lisp-unit:assert-first-fp-equal "0.294883390152d+02" (transport-5 4.0d0)))
