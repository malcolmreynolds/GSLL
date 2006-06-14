;********************************************************
; file:        fermi-dirac.lisp                          
; description: Fermi-Dirac function.                     
; date:        Sat Apr 22 2006 - 16:12                   
; author:      Liam M. Healy                             
; modified:    Tue Jun 13 2006 - 21:45
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Complete Fermi-Dirac Integrals
;;;;****************************************************************************

(defun-gsl fermi-dirac-m1 (x)
  "gsl_sf_fermi_dirac_m1_e" ((x :double) (ret sf-result))
  :documentation
  "The complete Fermi-Dirac integral with an index of @math{-1}. 
  This integral is given by @math{F_@{-1@}(x) = e^x / (1 + e^x)}.")

(defun-gsl fermi-dirac-0 (x)
  "gsl_sf_fermi_dirac_0_e" ((x :double) (ret sf-result))
  :documentation
  "The complete Fermi-Dirac integral with an index of @math{0}. 
   This integral is given by @math{F_0(x) = \ln(1 + e^x)}.")

(defun-gsl fermi-dirac-1 (x)
  "gsl_sf_fermi_dirac_1_e" ((x :double) (ret sf-result))
  :documentation
  "The complete Fermi-Dirac integral with an index of @math{1},
   @math{F_1(x) = \int_0^\infty dt (t /(\exp(t-x)+1))}.")

(defun-gsl fermi-dirac-2 (x)
  "gsl_sf_fermi_dirac_2_e" ((x :double) (ret sf-result))
  :documentation
  "The complete Fermi-Dirac integral with an index of @math{2},
  @math{F_2(x) = (1/2) \int_0^\infty dt (t^2 /(\exp(t-x)+1))}.")

(defun-gsl fermi-dirac-integral (j x)
  "gsl_sf_fermi_dirac_int_e" ((j :int) (x :double) (ret sf-result))
  :documentation
  "The complete Fermi-Dirac integral with an integer index of @math{j},
  @math{F_j(x) = (1/\Gamma(j+1)) \int_0^\infty dt (t^j /(\exp(t-x)+1))}.")

(defun-gsl fermi-dirac-m1/2 (x)
  "gsl_sf_fermi_dirac_mhalf_e" ((x :double) (ret sf-result))
  :documentation
  "The complete Fermi-Dirac integral @c{$F_{-1/2}(x)$}")

(defun-gsl fermi-dirac-1/2 (x)
  "gsl_sf_fermi_dirac_half_e" ((x :double) (ret sf-result))
  :documentation
  "The complete Fermi-Dirac integral @c{$F_{1/2}(x)$}.")

(defun-gsl fermi-dirac-3/2 (x)
  "gsl_sf_fermi_dirac_3half_e" ((x :double) (ret sf-result))
  :documentation
  "The complete Fermi-Dirac integral @c{$F_{3/2}(x)$}.")

;;;;****************************************************************************
;;;; Incomplete Fermi-Dirac Integrals
;;;;****************************************************************************

(defun-gsl fermi-dirac-inc-0 (x b)
  "gsl_sf_fermi_dirac_inc_0_e" ((x :double) (b :double) (ret sf-result))
  :documentation
  "The incomplete Fermi-Dirac integral with an index
  of zero, @c{$F_0(x,b) = \ln(1 + e^{b-x}) - (b-x)$}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test fermi-dirac
  (lisp-unit:assert-first-fp-equal
   "0.622459331202d+00"
   (fermi-dirac-m1 0.5d0))
  (lisp-unit:assert-first-fp-equal
   "0.113687100611d+01"
   (fermi-dirac-0 0.75d0))
  (lisp-unit:assert-first-fp-equal
   "0.425894306124d+00"   
   (fermi-dirac-1 -0.75d0))
  (lisp-unit:assert-first-fp-equal
   "0.113016301626d+01"
   (fermi-dirac-2 0.25d0))
  (lisp-unit:assert-first-fp-equal
   "0.666882708765d+04"
   (fermi-dirac-integral 5 12.35d0))
  (lisp-unit:assert-first-fp-equal
   "0.146429458909d+01"
   (fermi-dirac-m1/2 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.282372127740d+01"
   (fermi-dirac-1/2 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.416541445987d+01"
   (fermi-dirac-3/2 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.170141327798d+01"
   (fermi-dirac-inc-0 2.0d0 0.5d0)))
