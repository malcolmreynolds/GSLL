;********************************************************
; file:        exponential-integrals.lisp                
; description: Exponential integrals                     
; date:        Tue Mar 21 2006 - 17:37                   
; author:      Liam M. Healy                             
; modified:    Tue Jun 13 2006 - 21:40
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Exponential Integral
;;;;****************************************************************************

(defun-gsl expint-E1 (x)
  "gsl_sf_expint_E1_e" ((x :double) (ret sf-result))
  :documentation
  "The exponential integral
   @math{E_1(x)}, E_1(x) := \Re \int_1^\infty dt \exp(-xt)/t..")

(defun-gsl expint-E2 (x)
  "gsl_sf_expint_E2_e" ((x :double) (ret sf-result))
  :documentation
  "The second-order exponential integral
   @math{E_2(x)}, E_2(x) := \Re \int_1^\infty dt \exp(-xt)/t^2.")

;;;;****************************************************************************
;;;; Ei
;;;;****************************************************************************

(defun-gsl expint-Ei (x)
    "gsl_sf_expint_Ei_e" ((x :double) (ret sf-result))
  :documentation
  "The exponential integral @math{Ei(x)},
   Ei(x) := - PV\left(\int_{-x}^\infty dt \exp(-t)/t\right).")

;;;;****************************************************************************
;;;; Hyperbolic Integrals
;;;;****************************************************************************

(defun-gsl Shi (x)
  "gsl_sf_Shi_e" ((x :double) (ret sf-result))
  :documentation
  "The integral @math{Shi(x) = \int_0^x dt \sinh(t)/t}.")

(defun-gsl Chi (x)
  "gsl_sf_Chi_e" ((x :double) (ret sf-result))
  :documentation
  "The integral
   @math{ Chi(x) := \Re[ \gamma_E + \log(x) + \int_0^x dt (\cosh[t]-1)/t] },
   where @math{\gamma_E} is the Euler constant.")

;;;;****************************************************************************
;;;; Ei-3
;;;;****************************************************************************

(defun-gsl expint-3 (x)
  "gsl_sf_expint_3_e" ((x :double) (ret sf-result))
  :documentation
  "The third-order exponential integral @math{Ei_3(x) = \int_0^xdt \exp(-t^3)}
  for @math{x >= 0}.")

;;;;****************************************************************************
;;;; Trigonometric Integrals
;;;;****************************************************************************

(defun-gsl Si (x)
  "gsl_sf_Si_e" ((x :double) (ret sf-result))
  :documentation
  "The Sine integral @math{Si(x) = \int_0^x dt \sin(t)/t}.")

(defun-gsl Ci (x)
  "gsl_sf_Ci_e" ((x :double) (ret sf-result))
  :documentation
  "The Cosine integral @math{Ci(x) = -\int_x^\infty dt \cos(t)/t}
   for @math{x > 0}.")

;;;;****************************************************************************
;;;; Trigonometric Integrals
;;;;****************************************************************************

(defun-gsl atanint (x)
  "gsl_sf_atanint_e" ((x :double) (ret sf-result))
  :documentation
  "The Arctangent integral, which is defined as
   @math{AtanInt(x) = \int_0^x dt \arctan(t)/t}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test exponential-integrals
  (lisp-unit:assert-error 'gsl-error (expint-E1 0.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.219383934396d+00"
   (expint-E1 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.495423435600d+01"
   (expint-Ei 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.136373067344d+01"
   (Shi 1.25d0))
  (lisp-unit:assert-first-fp-equal
   "0.121731730091d+01"
   (Chi 1.25d0))
  (lisp-unit:assert-first-fp-equal
   "0.868892654126d+00"
   (expint-3 1.25d0))
  (lisp-unit:assert-first-fp-equal
   "0.114644641567d+01"
   (si 1.25d0))
  (lisp-unit:assert-first-fp-equal
   "0.434300724034d+00"
   (ci 1.25d0))
  (lisp-unit:assert-first-fp-equal
   "0.110361916168d+01"
   (atanint 1.25d0)))
