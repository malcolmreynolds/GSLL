;; Define the structures for solvers
;; Liam Healy 2009-06-06 16:46:38EDT solver-struct.lisp
;; Time-stamp: <2009-08-23 10:22:16EDT solver-struct.lisp>

(in-package :gsl)

#+linux
(define "_GNU_SOURCE")

;;; When installed through Mac Ports, GSL .h files will be found
;;; in /opt/local/include.
#+darwin
(cc-flags "-I/opt/local/include/")

(include "gsl/gsl_multifit_nlin.h")

;; The definition of a solver instance and state
;; for nonlinear least squares fitting in GSL."

(cstruct gsl-fdffit-solver "gsl_multifit_fdfsolver"
  (f "f" :type :pointer)
  (jacobian "J" :type :pointer)
  (dx "dx" :type :pointer))

(include "gsl/gsl_multiroots.h")

(cstruct gsl-multiroot-fsolver "gsl_multiroot_fsolver"
  (x "x" :type :pointer)
  (f "f" :type :pointer)
  (dx "dx" :type :pointer))
