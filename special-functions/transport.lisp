;; Transport functions
;; Liam Healy, Mon May  1 2006 - 22:29
;; Time-stamp: <2008-02-16 22:50:39EST transport.lisp>
;; $Id: $

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

#|
(make-tests transport
  (transport-2 4.0d0)
  (transport-3 4.0d0)
  (transport-4 4.0d0)
  (transport-5 4.0d0))
|#

(LISP-UNIT:DEFINE-TEST TRANSPORT
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.806666404563118d0 2.2867923780257255d-15)
   (MULTIPLE-VALUE-LIST (TRANSPORT-2 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 4.579217437229157d0 3.242324689309112d-15)
   (MULTIPLE-VALUE-LIST (TRANSPORT-3 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 10.731932392998623d0 1.0925209116254758d-14)
   (MULTIPLE-VALUE-LIST (TRANSPORT-4 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 29.488339015245842d0 3.204450601879883d-14)
   (MULTIPLE-VALUE-LIST (TRANSPORT-5 4.0d0))))
