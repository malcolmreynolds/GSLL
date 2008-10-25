;; Fermi-Dirac function.
;; Liam Healy, Sat Apr 22 2006 - 16:12
;; Time-stamp: <2008-10-23 22:58:29EDT fermi-dirac.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Complete Fermi-Dirac Integrals
;;;;****************************************************************************

(defmfun fermi-dirac-m1 (x)
  "gsl_sf_fermi_dirac_m1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The complete Fermi-Dirac integral with an index of -1. 
  This integral is given by F_{-1}(x) = e^x / (1 + e^x).")

(defmfun fermi-dirac-0 (x)
  "gsl_sf_fermi_dirac_0_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The complete Fermi-Dirac integral with an index of 0. 
   This integral is given by F_0(x) = \ln(1 + e^x).")

(defmfun fermi-dirac-1 (x)
  "gsl_sf_fermi_dirac_1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The complete Fermi-Dirac integral with an index of 1,
   F_1(x) = \int_0^\infty dt (t /(\exp(t-x)+1)).")

(defmfun fermi-dirac-2 (x)
  "gsl_sf_fermi_dirac_2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The complete Fermi-Dirac integral with an index of 2,
   F_2(x) = (1/2) \int_0^\infty dt (t^2 /(\exp(t-x)+1)).")

(defmfun fermi-dirac-integral (j x)
  "gsl_sf_fermi_dirac_int_e" ((j :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The complete Fermi-Dirac integral with an integer index of j,
   F_j(x) = (1/\Gamma(j+1)) \int_0^\infty dt (t^j /(\exp(t-x)+1)).")

(defmfun fermi-dirac-m1/2 (x)
  "gsl_sf_fermi_dirac_mhalf_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The complete Fermi-Dirac integral F_{-1/2}(x).")

(defmfun fermi-dirac-1/2 (x)
  "gsl_sf_fermi_dirac_half_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The complete Fermi-Dirac integral F_{1/2}(x).")

(defmfun fermi-dirac-3/2 (x)
  "gsl_sf_fermi_dirac_3half_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The complete Fermi-Dirac integral F_{3/2}(x).")

;;;;****************************************************************************
;;;; Incomplete Fermi-Dirac Integrals
;;;;****************************************************************************

(defmfun fermi-dirac-inc-0 (x b)
  "gsl_sf_fermi_dirac_inc_0_e" ((x :double) (b :double) (ret sf-result))
  :documentation			; FDL
  "The incomplete Fermi-Dirac integral with an index
  of zero, F_0(x,b) = \ln(1 + e^{b-x}) - (b-x).")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test fermi-dirac
  (fermi-dirac-m1 0.5d0)
  (fermi-dirac-0 0.75d0)
  (fermi-dirac-1 -0.75d0)
  (fermi-dirac-2 0.25d0)
  (fermi-dirac-integral 5 12.35d0)
  (fermi-dirac-m1/2 2.0d0)
  (fermi-dirac-1/2 2.0d0)
  (fermi-dirac-3/2 2.0d0)
  (fermi-dirac-inc-0 2.0d0 0.5d0))
