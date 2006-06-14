;********************************************************
; file:        error-functions.lisp                      
; description: Error functions                           
; date:        Mon Mar 20 2006 - 22:31                   
; author:      Liam M. Healy                             
; modified:    Tue Jun 13 2006 - 21:20
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl erf (x)
  "gsl_sf_erf_e" ((x :double) (ret sf-result))
  :documentation
  "The error function @math{erf(x)}, where
  @math{erf(x) = (2/\sqrt(\pi)) \int_0^x dt \exp(-t^2)}.")

(defun-gsl erfc (x)
  "gsl_sf_erfc_e" ((x :double) (ret sf-result))
  :documentation
  "The complementary error function 
  @math{erfc(x) = 1 - erf(x) = (2/\sqrt(\pi)) \int_x^\infty \exp(-t^2)}.")

(defun-gsl log-erfc (x)
  "gsl_sf_log_erfc_e" ((x :double) (ret sf-result))
  :documentation
  "The logarithm of the complementary error function @math{\log(\erfc(x))}.")

(defun-gsl erf-Z (x)
  "gsl_sf_erf_Z_e" ((x :double) (ret sf-result))
  :documentation
  "The Gaussian probability density function 
  @math{Z(x) = (1/\sqrt@{2\pi@}) \exp(-x^2/2)}.")

(defun-gsl erf-Q (x)
  "gsl_sf_erf_Q_e" ((x :double) (ret sf-result))
  :documentation
  "The upper tail of the Gaussian probability function 
  @math{Q(x) = (1/\sqrt@{2\pi@}) \int_x^\infty dt \exp(-t^2/2)}.")

(defun-gsl hazard (x)
  "gsl_sf_hazard_e" ((x :double) (ret sf-result))
  :documentation
  "The hazard function for the normal distribution.")

(lisp-unit:define-test error-functions
  (lisp-unit:assert-first-fp-equal "0.842700792950d+00" (erf 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.157299207050d+00" (erfc 1.0d0))
  (lisp-unit:assert-first-fp-equal "-0.184960550993d+01" (log-erfc 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.241970724519d+00" (erf-z 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.158655253931d+00" (erf-q 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.152513527616d+01" (hazard 1.0d0)))
