;; Physical constants in the MKSA system
;; Liam Healy 2009-05-25 17:01:32EDT mksa.lisp
;; Time-stamp: <2009-05-25 17:03:55EDT mksa.lisp>
;; $Id: $

#+linux
(define "_GNU_SOURCE")

(in-package :gsl)

(include "gsl/gsl_const_mksa.h")

(constant (+mksa-speed-of-light+ "GSL_CONST_MKSA_SPEED_OF_LIGHT"))
