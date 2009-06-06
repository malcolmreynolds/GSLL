;; CFFI-Grovel definitions.
;; Liam Healy
;; Time-stamp: <2009-06-06 11:01:18EDT eigen-struct.lisp>

#+linux
(define "_GNU_SOURCE")

(in-package :gsl)

(include "gsl/gsl_eigen.h")

(cstruct gsl-nonsymm-ws "gsl_eigen_nonsymm_workspace"
  (n-evals "n_evals" :type sizet))	     ; number of eigenvalues found
