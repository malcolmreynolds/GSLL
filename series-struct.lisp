;; Define the structure for series
;; Liam Healy 2009-06-06 11:08:11EDT series-struct.lisp
;; Time-stamp: <2009-06-06 16:44:41EDT series-struct.lisp>

#+linux
(define "_GNU_SOURCE")

(in-package :gsl)

(include "gsl/gsl_sum.h")

(cstruct levin-c "gsl_sum_levin_u_workspace"
	 (terms-used "terms_used" :type sizet)
	 (sum-plain "sum_plain" :type :double))
