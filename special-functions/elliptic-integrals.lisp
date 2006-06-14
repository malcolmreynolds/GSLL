;********************************************************
; file:        elliptic-integrals.lisp                   
; description: Elliptic integrals                        
; date:        Mon Mar 20 2006 - 21:50                   
; author:      Liam M. Healy                             
; modified:    Tue Jun 13 2006 - 21:09
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Legendre form of complete elliptic integrals
;;;;****************************************************************************

(defun-gsl elliptic-integral-K-complete (k)
  "gsl_sf_ellint_Kcomp_e" ((k :double) :mode (ret sf-result))
  :documentation
  "The complete elliptic integral @math{K(k)}.")

(defun-gsl elliptic-integral-E-complete (k)
  "gsl_sf_ellint_Ecomp_e" ((k :double) :mode (ret sf-result))
  :documentation "The complete elliptic integral @math{E(k)}.")

;;;;****************************************************************************
;;;; Legendre form of incomplete elliptic integrals
;;;;****************************************************************************

(defun-gsl elliptic-integral-F (phi k)
  "gsl_sf_ellint_F_e" ((phi :double) (k :double) :mode (ret sf-result))
  :documentation
  "The incomplete elliptic integral @math{F(\phi,k)}.")

(defun-gsl elliptic-integral-E (phi k)
  "gsl_sf_ellint_E_e" ((phi :double) (k :double) :mode (ret sf-result))
  :documentation
  "The incomplete elliptic integral @math{E(\phi,k)}.")

(defun-gsl elliptic-integral-P (phi k n)
  "gsl_sf_ellint_P_e"
  ((phi :double) (k :double) (n :double) :mode (ret sf-result))
  :documentation
  "The incomplete elliptic integral @math{P(\phi,k,n)}.")

(defun-gsl elliptic-integral-D (phi k n)
  "gsl_sf_ellint_D_e"
  ((phi :double) (k :double) (n :double) :mode (ret sf-result))
  :documentation
  "The incomplete elliptic integral @math{D(\phi,k,n)} which is
   defined through the Carlson form @math{RD(x,y,z)}
   by the following relation, 
   D(\phi,k,n) = RD (1-\sin^2(\phi), 1-k^2 \sin^2(\phi), 1).")

;;;;****************************************************************************
;;;; Carlson forms
;;;;****************************************************************************

(defun-gsl elliptic-integral-RC (x y)
  "gsl_sf_ellint_RC_e" ((x :double) (y :double) :mode (ret sf-result))
  :documentation
  "The incomplete elliptic integral @math{RC(x,y)}.")

(defun-gsl elliptic-integral-RD (x y z)
  "gsl_sf_ellint_RD_e"
  ((x :double) (y :double) (z :double) :mode (ret sf-result))
  :documentation
  "The incomplete elliptic integral @math{RD(x,y,z)}.")

(defun-gsl elliptic-integral-RF (x y z)
  "gsl_sf_ellint_RF_e"
  ((x :double) (y :double) (z :double) :mode (ret sf-result))
  :documentation
  "The incomplete elliptic integral @math{RF(x,y,z)}.")

(defun-gsl elliptic-integral-RJ (x y z p)
  "gsl_sf_ellint_RJ_e"
  ((x :double) (y :double) (z :double) (p :double) :mode (ret sf-result))
  :documentation
  "The incomplete elliptic integral @math{RJ(x,y,z,p)}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test elliptic-integrals
  (lisp-unit:assert-first-fp-equal "0.157079632679d+01"
				   (elliptic-integral-K-complete 0.0d0))
  (lisp-unit:assert-first-fp-equal "0.157079632679d+01"
				   (elliptic-integral-E-complete 0.0d0))
  (lisp-unit:assert-first-fp-equal "-0.677417538204d+00"
				   (elliptic-integral-F -0.5d0 2.0d0))
  (lisp-unit:assert-first-fp-equal "-0.401819480553d+00"
				   (elliptic-integral-E -0.5d0 2.0d0))
  (lisp-unit:assert-first-fp-equal "-0.617791316339d+00"
				   (elliptic-integral-P -0.5d0 2.0d0 1.0d0))
  (lisp-unit:assert-first-fp-equal "-0.688995144126d-01"
				   (elliptic-integral-D -0.5d0 2.0d0 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.881373587020d+00"
				   (elliptic-integral-RC 2.0d0 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.799259963030d+00"
				   (elliptic-integral-RD 2.0d0 1.0d0 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.881373587020d+00"
				   (elliptic-integral-RF 2.0d0 1.0d0 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.522800417499d+00"
				   (elliptic-integral-RJ 2.0d0 1.0d0 1.0d0 2.0d0)))
