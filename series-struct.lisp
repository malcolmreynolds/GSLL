;; Define the structure for series
;; Liam Healy 2009-06-06 11:08:11EDT series-struct.lisp
;; Time-stamp: <2009-08-23 10:22:16EDT series-struct.lisp>

(in-package :gsl)

#+linux
(define "_GNU_SOURCE")

;;; When installed through Mac Ports, GSL .h files will be found
;;; in /opt/local/include.
#+darwin
(cc-flags "-I/opt/local/include/")

(include "gsl/gsl_sum.h")

(cstruct levin-c "gsl_sum_levin_u_workspace"
	 (terms-used "terms_used" :type sizet)
	 (sum-plain "sum_plain" :type :double))
