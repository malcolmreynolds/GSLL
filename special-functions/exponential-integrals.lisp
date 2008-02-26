;; Exponential integrals
;; Liam Healy, Tue Mar 21 2006 - 17:37
;; Time-stamp: <2008-02-16 22:16:29EST exponential-integrals.lisp>
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

#|
;;; Macroexpanding in SLIME with slime-macroexpand-1 will produce the
;;; wrong error type for the first test.  Instead, evaluate in
;;; listener with (macroexpand-1 '(make-tests ... )).

(make-tests exponential-integrals
  (expint-E1 0.0d0)
  (expint-E1 1.0d0)
  (expint-Ei 2.0d0)
  (Shi 1.25d0)
  (Chi 1.25d0)
  (expint-3 1.25d0)
  (si 1.25d0)
  (ci 1.25d0)
  (atanint 1.25d0))
|#

(LISP-UNIT:DEFINE-TEST EXPONENTIAL-INTEGRALS
  (LISP-UNIT:ASSERT-ERROR 'GSL-ERROR (EXPINT-E1 0.0d0))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.21938393439552029d0 2.6541220085226265d-16)
   (MULTIPLE-VALUE-LIST (EXPINT-E1 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 4.954234356001891d0 7.64289440273947d-15)
   (MULTIPLE-VALUE-LIST (EXPINT-EI 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.363730673440693d0 4.275641706089105d-15)
   (MULTIPLE-VALUE-LIST (SHI 1.25d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.217317300914783d0 4.21062110717259d-15)
   (MULTIPLE-VALUE-LIST (CHI 1.25d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8688926541262327d0 4.083149724141479d-16)
   (MULTIPLE-VALUE-LIST (EXPINT-3 1.25d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.1464464156732344d0 7.36847290397885d-16)
   (MULTIPLE-VALUE-LIST (SI 1.25d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4343007240335524d0 1.2207688418479174d-15)
   (MULTIPLE-VALUE-LIST (CI 1.25d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.103619161676528d0 6.711588415833395d-16)
   (MULTIPLE-VALUE-LIST (ATANINT 1.25d0))))

