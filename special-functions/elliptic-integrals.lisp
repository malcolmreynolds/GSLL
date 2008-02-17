;; Elliptic integrals
;; Liam Healy, Mon Mar 20 2006 - 21:50
;; Time-stamp: <2008-02-16 20:47:06EST elliptic-integrals.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Legendre form of complete elliptic integrals
;;;;****************************************************************************

(defmfun elliptic-integral-K-complete (k)
  "gsl_sf_ellint_Kcomp_e" ((k :double) :mode (ret sf-result))
  :documentation			; FDL
  "The complete elliptic integral K(k).")

(defmfun elliptic-integral-E-complete (k)
  "gsl_sf_ellint_Ecomp_e" ((k :double) :mode (ret sf-result))
  :documentation			; FDL
  "The complete elliptic integral E(k).")

;;;;****************************************************************************
;;;; Legendre form of incomplete elliptic integrals
;;;;****************************************************************************

(defmfun elliptic-integral-F (phi k)
  "gsl_sf_ellint_F_e" ((phi :double) (k :double) :mode (ret sf-result))
  :documentation			; FDL
  "The incomplete elliptic integral F(\phi,k).")

(defmfun elliptic-integral-E (phi k)
  "gsl_sf_ellint_E_e" ((phi :double) (k :double) :mode (ret sf-result))
  :documentation			; FDL
  "The incomplete elliptic integral E(\phi,k).")

(defmfun elliptic-integral-P (phi k n)
  "gsl_sf_ellint_P_e"
  ((phi :double) (k :double) (n :double) :mode (ret sf-result))
  :documentation			; FDL
  "The incomplete elliptic integral P(\phi,k,n).")

(defmfun elliptic-integral-D (phi k n)
  "gsl_sf_ellint_D_e"
  ((phi :double) (k :double) (n :double) :mode (ret sf-result))
  :documentation			; FDL
  "The incomplete elliptic integral D(phi,k,n) which is
   defined through the Carlson form RD(x,y,z)
   by the following relation:
   D(phi,k,n) = RD (1-sin^2(phi), 1-k^2 sin^2(phi), 1).")

;;;;****************************************************************************
;;;; Carlson forms
;;;;****************************************************************************

(defmfun elliptic-integral-RC (x y)
  "gsl_sf_ellint_RC_e" ((x :double) (y :double) :mode (ret sf-result))
  :documentation			; FDL
  "The incomplete elliptic integral RC(x,y).")

(defmfun elliptic-integral-RD (x y z)
  "gsl_sf_ellint_RD_e"
  ((x :double) (y :double) (z :double) :mode (ret sf-result))
  :documentation			; FDL
  "The incomplete elliptic integral RD(x,y,z).")

(defmfun elliptic-integral-RF (x y z)
  "gsl_sf_ellint_RF_e"
  ((x :double) (y :double) (z :double) :mode (ret sf-result))
  :documentation			; FDL
  "The incomplete elliptic integral RF(x,y,z).")

(defmfun elliptic-integral-RJ (x y z p)
  "gsl_sf_ellint_RJ_e"
  ((x :double) (y :double) (z :double) (p :double) :mode (ret sf-result))
  :documentation			; FDL
  "The incomplete elliptic integral RJ(x,y,z,p).")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests elliptic-integrals
  (elliptic-integral-K-complete 0.0d0)
  (elliptic-integral-E-complete 0.0d0)
  (elliptic-integral-F -0.5d0 2.0d0)
  (elliptic-integral-E -0.5d0 2.0d0)
  (elliptic-integral-P -0.5d0 2.0d0 1.0d0)
  (elliptic-integral-D -0.5d0 2.0d0 1.0d0)
  (elliptic-integral-RC 2.0d0 1.0d0)
  (elliptic-integral-RD 2.0d0 1.0d0 1.0d0)
  (elliptic-integral-RF 2.0d0 1.0d0 1.0d0)
  (elliptic-integral-RJ 2.0d0 1.0d0 1.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST ELLIPTIC-INTEGRALS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.5707963267948966d0 4.598091522633788d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-K-COMPLETE 0.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.5707963267948966d0 3.487868498008632d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-E-COMPLETE 0.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.6774175382039307d0 3.008338192795582d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-F -0.5d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.4018194805534952d0 4.232239429377521d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-E -0.5d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.617791316339182d0 1.6365659051690947d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-P -0.5d0 2.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.06889951441260889d0 3.059753091454848d-17)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-D -0.5d0 2.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8813735870195432d0 1.9570424992111216d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-RC 2.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.7992599630303281d0 1.7747136272346433d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-RD 2.0d0 1.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8813735870195432d0 1.9570424992111216d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-RF 2.0d0 1.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5228004174989865d0 1.160850121582039d-16)
   (MULTIPLE-VALUE-LIST
    (ELLIPTIC-INTEGRAL-RJ 2.0d0 1.0d0 1.0d0 2.0d0))))
