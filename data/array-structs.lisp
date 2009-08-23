;; CFFI-Grovel definitions for unix systems.
;; Liam Healy 2009-06-06 09:36:29EDT array-structs.lisp
;; Time-stamp: <2009-08-23 10:15:51EDT array-structs.lisp>

(in-package :gsl)

#+linux
(define "_GNU_SOURCE")

;;; When installed through Mac Ports, GSL .h files will be found
;;; in /opt/local/include.
#+darwin
(cc-flags "-I/opt/local/include/")

(include "gsl/gsl_block_double.h")

(cstruct gsl-block-c "gsl_block"
  (size "size" :type sizet)
  (data "data" :type :pointer))

(include "gsl/gsl_vector_double.h")

(cstruct gsl-vector-c "gsl_vector"
  (size "size" :type sizet)
  (stride "stride" :type sizet)
  (data "data" :type :pointer)
  (block "block" :type :pointer)
  (owner "owner" :type :int))

(include "gsl/gsl_matrix_double.h")

(cstruct gsl-matrix-c "gsl_matrix"
  (size0 "size1" :type sizet)
  (size1 "size2" :type sizet)
  (tda "tda" :type sizet)
  (data "data" :type :pointer)
  (block "block" :type :pointer)
  (owner "owner" :type :int))

(include "gsl/gsl_permutation.h")

(cstruct gsl-permutation-c "gsl_permutation"
  (size "size" :type sizet)
  (data "data" :type :pointer))

(include "gsl/gsl_combination.h")

(cstruct gsl-combination-c "gsl_combination"
  (choice-of "n" :type sizet)
  (size "k" :type sizet)
  (data "data" :type :pointer))
