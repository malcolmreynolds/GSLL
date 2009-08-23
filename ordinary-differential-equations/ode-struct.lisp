;; Define the structure for ODEs
;; Liam Healy 2009-06-06 16:42:29EDT ode-struct.lisp
;; Time-stamp: <2009-08-23 10:20:07EDT ode-struct.lisp>

(in-package :gsl)

#+linux
(define "_GNU_SOURCE")

;;; When installed through Mac Ports, GSL .h files will be found
;;; in /opt/local/include.
#+darwin
(cc-flags "-I/opt/local/include/")

(include "gsl/gsl_odeiv.h")

(cstruct ode-system "gsl_odeiv_system"
  (function "function" :type :pointer) 
  (jacobian "jacobian" :type :pointer)
  (dimension "dimension" :type sizet)
  (parameters "params" :type :pointer))
