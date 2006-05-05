;********************************************************
; file:        transport.lisp                                
; description: Transport functions
; date:        Mon May  1 2006 - 22:29
; author:      Liam M. Healy                             
; modified:    Thu May  4 2006 - 23:18
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; The transport functions @math{J(n,x)} are defined by the integral 
;;; representations
;;; @c{$J(n,x) := \int_0^x dt \, t^n e^t /(e^t - 1)^2$}
;;; @math{J(n,x) := \int_0^x dt t^n e^t /(e^t - 1)^2}.

(defun-gsl transport-2 ((x :double))
  "gsl_sf_transport_2_e"
  :documentation "The transport function @math{J(2,x)}."
  :return (sf-result))

(defun-gsl transport-3 ((x :double))
  "gsl_sf_transport_3_e"
  :documentation "The transport function @math{J(3,x)}."
  :return (sf-result))

(defun-gsl transport-4 ((x :double))
  "gsl_sf_transport_4_e"
  :documentation "The transport function @math{J(4,x)}."
  :return (sf-result))

(defun-gsl transport-5 ((x :double))
  "gsl_sf_transport_5_e"
  :documentation "The transport function @math{J(5,x)}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test transport
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.280666640456d+01" (TRANSPORT-2 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.457921743723d+01" (TRANSPORT-3 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.107319323930d+02" (TRANSPORT-4 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.294883390152d+02" (TRANSPORT-5 4.0d0)))
