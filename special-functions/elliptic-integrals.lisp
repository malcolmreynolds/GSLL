;********************************************************
; file:        elliptic-integrals.lisp                   
; description: Elliptic integrals                        
; date:        Mon Mar 20 2006 - 21:50                   
; author:      Liam M. Healy                             
; modified:    Mon Mar 20 2006 - 22:15
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Legendre form of complete elliptic integrals
;;;;****************************************************************************

(defun-sf elliptic-integral-K-complete ((k :double))
  "gsl_sf_ellint_Kcomp_e"
  :documentation
  "The complete elliptic integral @math{K(k)}."
  :mode t
  :return (sf-result))

(defun-sf elliptic-integral-E-complete ((k :double))
  "gsl_sf_ellint_Ecomp_e"
  :documentation
  "The complete elliptic integral @math{E(k)}."
  :mode t
  :return (sf-result))

;;;;****************************************************************************
;;;; Legendre form of incomplete elliptic integrals
;;;;****************************************************************************

(defun-sf elliptic-integral-F ((phi :double) (k :double))
  "gsl_sf_ellint_F_e"
  :documentation
  "The incomplete elliptic integral @math{F(\phi,k)}."
  :mode t
  :return (sf-result))

(defun-sf elliptic-integral-E ((phi :double) (k :double))
  "gsl_sf_ellint_E_e"
  :documentation
  "The incomplete elliptic integral @math{E(\phi,k)}."
  :mode t
  :return (sf-result))

(defun-sf elliptic-integral-P ((phi :double) (k :double) (n :double))
  "gsl_sf_ellint_P_e"
  :documentation
  "The incomplete elliptic integral @math{P(\phi,k,n)}."
  :mode t
  :return (sf-result))

(defun-sf elliptic-integral-D ((phi :double) (k :double) (n :double))
  "gsl_sf_ellint_D_e"
  :documentation
  "The incomplete elliptic integral
@math{D(\phi,k,n)} which is defined through the Carlson form @math{RD(x,y,z)}
by the following relation, 
@tex
\beforedisplay
$$
D(\phi,k,n) = RD (1-\sin^2(\phi), 1-k^2 \sin^2(\phi), 1).
$$
\afterdisplay
@end tex"
  :mode t
  :return (sf-result))

;;;;****************************************************************************
;;;; Carlson forms
;;;;****************************************************************************

(defun-sf elliptic-integral-RC ((x :double) (y :double))
  "gsl_sf_ellint_RC_e"
  :documentation
  "The incomplete elliptic integral @math{RC(x,y)}."
  :mode t
  :return (sf-result))

(defun-sf elliptic-integral-RD ((x :double) (y :double) (z :double))
  "gsl_sf_ellint_RD_e"
  :documentation
  "The incomplete elliptic integral @math{RD(x,y,z)}."
  :mode t
  :return (sf-result))

(defun-sf elliptic-integral-RF ((x :double) (y :double) (z :double))
  "gsl_sf_ellint_RF_e"
  :documentation
  "The incomplete elliptic integral @math{RF(x,y,z)}."
  :mode t
  :return (sf-result))

(defun-sf elliptic-integral-RJ ((x :double) (y :double) (z :double) (p :double))
  "gsl_sf_ellint_RJ_e"
  :documentation
  "The incomplete elliptic integral @math{RJ(x,y,z,p)}."
  :mode t
  :return (sf-result))
