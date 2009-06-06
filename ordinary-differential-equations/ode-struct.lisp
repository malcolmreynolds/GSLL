;; Define the structure for ODEs
;; Liam Healy 2009-06-06 16:42:29EDT ode-struct.lisp
;; Time-stamp: <2009-06-06 17:08:02EDT ode-struct.lisp>

#+linux
(define "_GNU_SOURCE")

(in-package :gsl)

(include "gsl/gsl_odeiv.h")

(cstruct ode-system "gsl_odeiv_system"
  (function "function" :type :pointer) 
  (jacobian "jacobian" :type :pointer)
  (dimension "dimension" :type sizet)
  (parameters "params" :type :pointer))
