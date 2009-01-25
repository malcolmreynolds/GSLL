;; Discrete Hankel Transforms.
;; Liam Healy, Sat Dec  8 2007 - 16:50
;; Time-stamp: <2009-01-25 10:00:41EST hankel.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_dht.h

;;; Everything compiles, but not tested -- need example.
;;; We don't use gsl_dht_new = gsl_dht_alloc + gsl_dht_init.

;;; Create the Hankel transform object and essential methods/functions.
(defmobject hankel "gsl_dht"
  ((size sizet))
  "discrete Hankel Transform"
  :documentation
  "Allocate a Discrete Hankel transform object of the given size and
   optionally initialize the transform for the given values of nu and x."
  :initialize-suffix "init"
  :initialize-args ((nu :double) (xmax :double)))

(defmfun apply-hankel (hankel array-in array-out)
  "gsl_dht_apply"
  (((mpointer hankel) :pointer) ((c-pointer array-in) :pointer)
   ((c-pointer array-out) :pointer))
  :documentation			; FDL
  "Apply the transform to the array array-in
   whose size is equal to the size of the transform.  The result is stored
   in the array array-out which must be of the same length.")

(defmfun sample-x-hankel (hankel n)
  "gsl_dht_x_sample"
  (((mpointer hankel) :pointer) (n :int))
  :c-return :double
  :documentation			; FDL
  "The points where the function f(t) is assumed to be sampled.")

(defmfun sample-k-hankel (hankel n)
  "gsl_dht_k_sample"
  (((mpointer hankel) :pointer) (n :int))
  :c-return :double
  :documentation			; FDL
  "The value of the n-th sample point in k-space, j_{\nu,n+1}/X}.")
