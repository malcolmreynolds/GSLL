;; Exponential integrals
;; Liam Healy, Tue Mar 21 2006 - 17:37
;; Time-stamp: <2008-11-16 12:07:43EST exponential-integrals.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Exponential Integral
;;;;****************************************************************************

(defmfun expint-E1 (x)
  "gsl_sf_expint_E1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The exponential integral
   E_1(x)}, E_1(x) := \Re \int_1^\infty dt \exp(-xt)/t..")

(defmfun expint-E2 (x)
  "gsl_sf_expint_E2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The second-order exponential integral
   E_2(x)}, E_2(x) := \Re \int_1^\infty dt \exp(-xt)/t^2.")

;;;;****************************************************************************
;;;; Ei
;;;;****************************************************************************

(defmfun expint-Ei (x)
    "gsl_sf_expint_Ei_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The exponential integral Ei(x),
   Ei(x) := - PV\left(\int_{-x}^\infty dt \exp(-t)/t\right).")

;;;;****************************************************************************
;;;; Hyperbolic Integrals
;;;;****************************************************************************

(defmfun Shi (x)
  "gsl_sf_Shi_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The integral Shi(x) = \int_0^x dt \sinh(t)/t.")

(defmfun Chi (x)
  "gsl_sf_Chi_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The integral
   Chi(x) := \Re[ \gamma_E + \log(x) + \int_0^x dt (\cosh[t]-1)/t],
   where \gamma_E} is the Euler constant.")

;;;;****************************************************************************
;;;; Ei-3
;;;;****************************************************************************

(defmfun expint-3 (x)
  "gsl_sf_expint_3_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The third-order exponential integral Ei_3(x) = \int_0^xdt \exp(-t^3)
  for x >= 0.")

;;;;****************************************************************************
;;;; Trigonometric Integrals
;;;;****************************************************************************

(defmfun Si (x)
  "gsl_sf_Si_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Sine integral Si(x) = \int_0^x dt \sin(t)/t.")

(defmfun Ci (x)
  "gsl_sf_Ci_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Cosine integral Ci(x) = -\int_x^\infty dt \cos(t)/t
   for x > 0.")

;;;;****************************************************************************
;;;; Trigonometric Integrals
;;;;****************************************************************************

(defmfun atanint (x)
  "gsl_sf_atanint_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Arctangent integral, which is defined as
   AtanInt(x) = \int_0^x dt \arctan(t)/t.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test exponential-integrals
  (expint-E1 0.0d0)
  (expint-E1 1.0d0)
  (expint-Ei 2.0d0)
  (Shi 1.25d0)
  (Chi 1.25d0)
  (expint-3 1.25d0)
  (si 1.25d0)
  (ci 1.25d0)
  (atanint 1.25d0))
