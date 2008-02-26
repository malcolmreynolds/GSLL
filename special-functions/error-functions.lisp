;; Error functions
;; Liam Healy, Mon Mar 20 2006 - 22:31
;; Time-stamp: <2008-02-16 20:54:49EST error-functions.lisp>
;; $Id$

(in-package :gsl)

(defmfun erf (x)
  "gsl_sf_erf_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The error function erf(x), where
  erf(x) = (2/\sqrt(\pi)) \int_0^x dt \exp(-t^2).")

(defmfun erfc (x)
  "gsl_sf_erfc_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The complementary error function 
  erfc(x) = 1 - erf(x) = (2/\sqrt(\pi)) \int_x^\infty \exp(-t^2).")

(defmfun log-erfc (x)
  "gsl_sf_log_erfc_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The logarithm of the complementary error function \log(\erfc(x)).")

(defmfun erf-Z (x)
  "gsl_sf_erf_Z_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Gaussian probability density function 
  Z(x) = (1/sqrt{2\pi}) \exp(-x^2/2)}.")

(defmfun erf-Q (x)
  "gsl_sf_erf_Q_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The upper tail of the Gaussian probability function 
  Q(x) = (1/\sqrt{2\pi}) \int_x^\infty dt \exp(-t^2/2)}.")

(defmfun hazard (x)
  "gsl_sf_hazard_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The hazard function for the normal distribution.")

#|
(make-tests error-functions
  (erf 1.0d0)
  (erfc 1.0d0)
  (log-erfc 1.0d0)
  (erf-z 1.0d0)
  (erf-q 1.0d0)
  (hazard 1.0d0))
|#

(LISP-UNIT:DEFINE-TEST ERROR-FUNCTIONS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8427007929497149d0 7.789237746491556d-16)
   (MULTIPLE-VALUE-LIST (ERF 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1572992070502851d0 4.0468944536809554d-16)
   (MULTIPLE-VALUE-LIST (ERFC 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -1.8496055099332485d0 3.394126565390616d-15)
   (MULTIPLE-VALUE-LIST (LOG-ERFC 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.24197072451914334d0 1.611848817878303d-16)
   (MULTIPLE-VALUE-LIST (ERF-Z 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.15865525393145707d0 2.832400331480832d-16)
   (MULTIPLE-VALUE-LIST (ERF-Q 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.5251352761609807d0 5.532094155354489d-15)
   (MULTIPLE-VALUE-LIST (HAZARD 1.0d0))))

