;********************************************************
; file:        error-functions.lisp                      
; description: Error functions                           
; date:        Mon Mar 20 2006 - 22:31                   
; author:      Liam M. Healy                             
; modified:    Sun May 21 2006 - 19:08
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl erf ((x :double))
  "gsl_sf_erf_e"
  :documentation
  "The error function @c{$\erf(x)$}
@math{erf(x)}, where
@c{$\erf(x) = (2/\sqrt{\pi}) \int_0^x dt \exp(-t^2)$}
@math{erf(x) = (2/\sqrt(\pi)) \int_0^x dt \exp(-t^2)}."
  :return (sf-result))

(defun-gsl erfc ((x :double))
  "gsl_sf_erfc_e"
  :documentation
  "The complementary error function 
@c{$\erfc(x) = 1 - \erf(x) = (2/\sqrt{\pi}) \int_x^\infty \exp(-t^2)$}
@math{erfc(x) = 1 - erf(x) = (2/\sqrt(\pi)) \int_x^\infty \exp(-t^2)}."
  :return (sf-result))

(defun-gsl log-erfc ((x :double))
  "gsl_sf_log_erfc_e"
  :documentation
  "The logarithm of the complementary error function @math{\log(\erfc(x))}."
  :return (sf-result))

(defun-gsl erf-Z ((x :double))
  "gsl_sf_erf_Z_e"
  :documentation
  "The Gaussian probability density function 
@c{$Z(x) = (1/\sqrt{2\pi}) \exp(-x^2/2)$} 
@math{Z(x) = (1/\sqrt@{2\pi@}) \exp(-x^2/2)}."
  :return (sf-result))

(defun-gsl erf-Q ((x :double))
  "gsl_sf_erf_Q_e"
  :documentation
"The upper tail of the Gaussian probability
function 
@c{$Q(x) = (1/\sqrt{2\pi}) \int_x^\infty dt \exp(-t^2/2)$}
@math{Q(x) = (1/\sqrt@{2\pi@}) \int_x^\infty dt \exp(-t^2/2)}."
  :return (sf-result))

(defun-gsl hazard ((x :double))
  "gsl_sf_hazard_e"
  :documentation
  "The hazard function for the normal distribution."
  :return (sf-result))

(lisp-unit:define-test error-functions
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.842700792950d+00" (ERF 1.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.157299207050d+00" (ERFC 1.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "-0.184960550993d+01" (LOG-ERFC 1.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.241970724519d+00" (ERF-Z 1.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.158655253931d+00" (ERF-Q 1.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.152513527616d+01" (HAZARD 1.0d0)))
