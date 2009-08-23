;; CFFI-Grovel definitions.
;; Liam Healy
;; Time-stamp: <2009-08-23 10:18:04EDT eigen-struct.lisp>

(in-package :gsl)

#+linux
(define "_GNU_SOURCE")

;;; When installed through Mac Ports, GSL .h files will be found
;;; in /opt/local/include.
#+darwin
(cc-flags "-I/opt/local/include/")

(include "gsl/gsl_eigen.h")

(cstruct gsl-nonsymm-ws "gsl_eigen_nonsymm_workspace"
  (n-evals "n_evals" :type sizet))	     ; number of eigenvalues found
