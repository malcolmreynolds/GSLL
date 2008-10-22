;; Deybe functions
;; Liam Healy, Sun Mar 19 2006 - 14:34
;; Time-stamp: <2008-10-21 22:55:48EDT debye.lisp>
;; $Id$

#|
;;; FDL
The Debye functions D_n(x) are defined by the following integral,
D_n(x) = n/x^n \int_0^x dt (t^n/(e^t - 1))
For further information see Abramowitz &
Stegun, Section 27.1.
|#

(in-package :gsl)

(defmfun debye-1 (x)
  "gsl_sf_debye_1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The first-order Debye function D_1(x) = (1/x) \int_0^x dt (t/(e^t - 1)).")

(defmfun debye-2 (x)
  "gsl_sf_debye_2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The second-order Debye function
   D_2(x) = (2/x^2) \int_0^x dt (t^2/(e^t - 1)).")

(defmfun debye-3 (x)
  "gsl_sf_debye_3_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The third-order Debye function
   D_3(x) = (3/x^3) \int_0^x dt (t^3/(e^t - 1)).")

(defmfun debye-4 (x)
  "gsl_sf_debye_4_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The fourth-order Debye function
  D_4(x) = (4/x^4) \int_0^x dt (t^4/(e^t - 1)).")

(save-test debye
  (debye-1 1.0d0)
  (debye-2 1.0d0)
  (debye-3 1.0d0)
  (debye-4 1.0d0))

