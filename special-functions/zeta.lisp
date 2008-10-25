;; Zeta functions
;; Liam Healy, Sat May 13 2006 - 23:27
;; Time-stamp: <2008-10-25 11:31:03EDT zeta.lisp>
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

(save-test zeta
  (zeta 2)
  (zeta 1.5d0)
  (zeta-1 2)
  (zeta-1 1.5d0)
  (hurwitz-zeta 1.5d0 2.5d0)
  (eta 1.5d0))

