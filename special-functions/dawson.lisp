;; Dawson function
;; Liam Healy, Sun Mar 19 2006 - 14:31
;; Time-stamp: <2008-02-16 20:04:51EST dawson.lisp>
;; $Id$

#|
;;; FDL
The Dawson integral is defined by \exp(-x^2) \int_0^x dt
\exp(t^2).  A table of Dawson's integral can be found in Abramowitz &
Stegun, Table 7.5.  The Dawson functions are declared in the header file
gsl_sf_dawson.h.
|#

(in-package :gsl)

(defmfun dawson (x)
  "gsl_sf_dawson_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "Dawson's integral for x.")

#|
(make-tests dawson
	    (dawson 1.0d0))
|#

(LISP-UNIT:DEFINE-TEST DAWSON
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5380795069127684d0 1.424354102650492d-15)
   (MULTIPLE-VALUE-LIST (DAWSON 1.0d0))))

