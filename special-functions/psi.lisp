;; Psi (digamma) functions
;; Liam Healy, Mon May  1 2006 - 22:11
;; Time-stamp: <2008-10-25 11:27:13EDT psi.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Digamma Function
;;;;****************************************************************************

(defgeneric psi (x)
  ;; FDL
  (:documentation "The psi, or digamma, function."))

(defmfun psi  ((n integer))
  "gsl_sf_psi_int_e" ((n :int) (ret sf-result))
  :definition :method
  :export t
  :documentation			; FDL
  "Domain: n integer, n > 0.")

(defmfun psi ((x float))
  "gsl_sf_psi_e" ((x :double) (ret sf-result))
  :definition :method 
  :documentation			; FDL
  "Domain: x /= 0.0, -1.0, -2.0, ...")

(defmfun psi-1+iy (x)
  "gsl_sf_psi_1piy_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The real part of the digamma function
  on the line 1+i y, Re[psi(1 + i y)].")

;;;;****************************************************************************
;;;; Trigamma Function
;;;;****************************************************************************

(defgeneric psi-1 (x)
  ;; FDL
  (:documentation "The Trigamma function."))

(defmfun psi-1 ((n integer))
  "gsl_sf_psi_1_int_e" ((n :int) (ret sf-result))
  :definition :method 
  :documentation			; FDL
  "Domain: n integer, n > 0.")

(defmfun psi-1 ((x float))
  "gsl_sf_psi_1_e" ((x :double) (ret sf-result))
  :definition :method
  :documentation			; FDL
  "Domain: x /= 0.0, -1.0, -2.0, ...")

;;;;****************************************************************************
;;;; Polygamma
;;;;****************************************************************************

(defmfun psi-n (m x)
  "gsl_sf_psi_n_e" ((m :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The polygamma function psi^{(m)}(x)} for m >= 0, x > 0.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test psi
  (psi 4)
  (psi 4.0d0)
  (psi-1+iy 2.0d0)
  (psi-1 4)
  (psi-1 4.0d0)
  (psi-n 2 4.0d0))

