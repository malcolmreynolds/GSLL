;; Transport functions
;; Liam Healy, Mon May  1 2006 - 22:29
;; Time-stamp: <2008-10-25 11:30:06EDT transport.lisp>
;; $Id$

(in-package :gsl)

;;;  FDL
;;; The transport functions J(n,x) are defined by the integral 
;;; representations
;;; J(n,x) := \int_0^x dt \, t^n e^t /(e^t - 1)^2.

(defmfun transport-2 (x)
  "gsl_sf_transport_2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The transport function J(2,x).")

(defmfun transport-3 (x)
  "gsl_sf_transport_3_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The transport function J(3,x).")

(defmfun transport-4 (x)
  "gsl_sf_transport_4_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The transport function J(4,x).")

(defmfun transport-5 (x)
  "gsl_sf_transport_5_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The transport function J(5,x).")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test transport
  (transport-2 4.0d0)
  (transport-3 4.0d0)
  (transport-4 4.0d0)
  (transport-5 4.0d0))

