;********************************************************
; file:        exponential-integrals.lisp                
; description: Exponential integrals                     
; date:        Tue Mar 21 2006 - 17:37                   
; author:      Liam M. Healy                             
; modified:    Tue Mar 21 2006 - 17:48
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Exponential Integral
;;;;****************************************************************************

(defun-sf expint-E1 ((x :double))
  "gsl_sf_expint_E1_e"
  :documentation
  "The exponential integral @math{E_1(x)}, E_1(x) := \Re \int_1^\infty dt \exp(-xt)/t.."
  :return (sf-result))

(defun-sf expint-E2 ((x :double))
  "gsl_sf_expint_E2_e"
  :documentation
  "The second-order exponential integral @math{E_2(x)}, E_2(x) := \Re \int_1^\infty dt \exp(-xt)/t^2."
  :return (sf-result))

;;;;****************************************************************************
;;;; Ei
;;;;****************************************************************************

(defun-sf expint-Ei ((x :double))
  "gsl_sf_expint_Ei_e"
  :documentation
  "The exponential integral @math{Ei(x)}, Ei(x) := - PV\left(\int_{-x}^\infty dt \exp(-t)/t\right)."
  :return (sf-result))

;;;;****************************************************************************
;;;; Hyperbolic Integrals
;;;;****************************************************************************

(defun-sf Shi ((x :double))
  "gsl_sf_Shi_e"
  :documentation
  "The integral @math{Shi(x) = \int_0^x dt \sinh(t)/t}."
  :return (sf-result))

(defun-sf Chi ((x :double))
  "gsl_sf_Chi_e"
  :documentation
  "The integral @math{ Chi(x) := \Re[ \gamma_E + \log(x) + \int_0^x dt (\cosh[t]-1)/t] }, where @math{\gamma_E} is the Euler constant."
  :return (sf-result))

;;;;****************************************************************************
;;;; Ei-3
;;;;****************************************************************************

(defun-sf expint-3 ((x :double))
  "gsl_sf_expint_3_e"
  :documentation
  "The third-order exponential integral @math{Ei_3(x) = \int_0^xdt \exp(-t^3)} for @c{$x \ge 0$} @math{x >= 0}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Trigonometric Integrals
;;;;****************************************************************************

(defun-sf Si ((x :double))
  "gsl_sf_Si_e"
  :documentation
  "The Sine integral @math{Si(x) = \int_0^x dt \sin(t)/t}."
  :return (sf-result))

(defun-sf Ci ((x :double))
  "gsl_sf_Ci_e"
  :documentation
  "The Cosine integral @math{Ci(x) = -\int_x^\infty dt \cos(t)/t} for @math{x > 0}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Trigonometric Integrals
;;;;****************************************************************************

(defun-sf atanint ((x :double))
  "gsl_sf_atanint_e"
  :documentation
  "The Arctangent integral, which is defined as @math{AtanInt(x) = \int_0^x dt \arctan(t)/t}."
  :return (sf-result))
