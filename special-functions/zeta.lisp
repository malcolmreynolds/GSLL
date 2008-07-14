;; Zeta functions
;; Liam Healy, Sat May 13 2006 - 23:27
;; Time-stamp: <2008-07-12 13:40:50EDT zeta.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Riemann Zeta Function
;;;;****************************************************************************

;;; FDL
;;; The Riemann zeta function is defined by the infinite sum 
;;; zeta(s) = \sum_{k=1}^\infty k^{-s}.  

(defgeneric zeta (x)
  (:documentation			; FDL
   "The Riemann zeta function zeta(n)."))

(defmfun zeta ((n integer))
  "gsl_sf_zeta_int_e" ((n :int) (ret sf-result))
  :definition :method
  :export t
  :documentation			; FDL
  "The Riemann zeta function zeta(n) for integer n, n \ne 1.")

(defmfun zeta ((s float))
  "gsl_sf_zeta_e" ((s :double) (ret sf-result))
  :definition :method
  :documentation			; FDL
  "The Riemann zeta function zeta(s) for arbitrary s, s \ne 1.")

;;;;****************************************************************************
;;;; Riemann Zeta Function Minus One
;;;;****************************************************************************

(defgeneric zeta-1 (x)
  (:documentation "zeta - 1."))

(defmfun zeta-1 ((n integer))
  "gsl_sf_zetam1_int_e" ((n :int) (ret sf-result))
  :definition :method
  :export t
  :documentation			; FDL
  "The Riemann zeta function zeta(n) for integer n, n \ne 1.")

(defmfun zeta-1 ((s float))
  "gsl_sf_zetam1_e" ((s :double) (ret sf-result))
  :definition :method
  :documentation			; FDL
  "The Riemann zeta function zeta(s) for arbitrary s, s \ne 1.")

;;;;****************************************************************************
;;;; Hurwitz Zeta Function
;;;;****************************************************************************

(defmfun hurwitz-zeta (s q)
  "gsl_sf_hzeta_e" ((s :double) (q :double) (ret sf-result))
  :documentation			; FDL
  "The Hurwitz zeta function zeta(s,q) for s > 1, q > 0.")

;;;;****************************************************************************
;;;; Eta Function
;;;;****************************************************************************

(defmfun eta (s)
  "gsl_sf_eta_e" ((s :double) (ret sf-result))
  :documentation			; FDL
  "The eta function eta(s) for arbitrary s.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests zeta
  (zeta 2)
  (zeta 1.5d0)
  (zeta-1 2)
  (zeta-1 1.5d0)
  (hurwitz-zeta 1.5d0 2.5d0)
  (eta 1.5d0))
|#

(LISP-UNIT:DEFINE-TEST ZETA
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.6449340668482264d0 7.304974700020789d-16)
   (MULTIPLE-VALUE-LIST (ZETA 2)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.612375348685493d0 1.419471177334903d-14)
   (MULTIPLE-VALUE-LIST (ZETA 1.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6449340668482264d0 2.8640826015201633d-16)
   (MULTIPLE-VALUE-LIST (ZETA-1 2)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.612375348685493d0 1.419471177334903d-14)
   (MULTIPLE-VALUE-LIST (ZETA-1 1.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.4037797688568256d0 8.104244828616706d-15)
   (MULTIPLE-VALUE-LIST (HURWITZ-ZETA 1.5d0 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.7651470246254092d0 1.0661276646941275d-14)
   (MULTIPLE-VALUE-LIST (ETA 1.5d0))))
