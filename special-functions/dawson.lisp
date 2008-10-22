;; Dawson function
;; Liam Healy, Sun Mar 19 2006 - 14:31
;; Time-stamp: <2008-10-21 22:49:36EDT dawson.lisp>
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

(save-test dawson (dawson 1.0d0))


