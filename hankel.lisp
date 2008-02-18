;; Discrete Hankel Transforms.
;; Liam Healy, Sat Dec  8 2007 - 16:50
;; Time-stamp: <2008-02-17 18:10:15EST hankel.lisp>
;; $Id: $

(in-package :gsl)

;;; Everything compiles, but not tested -- need example.

(defgo-s (hankel size nu xmax) allocate-hankel free-hankel init-hankel)

(defmfun allocate-hankel (size)
  "gsl_dht_alloc"
  ((size size))
  :c-return :pointer
  :export nil
  :index (letm hankel)
  :documentation			; FDL
  "Allocate a Discrete Hankel transform object of given size.")

(defmfun init-hankel (hankel nu xmax)
  "gsl_dht_new"
  ((hankel :pointer) (nu :double) (xmax :double))
  :export nil
  :index (letm hankel)
  :documentation			; FDL
  "Initialize the transform for the given values of nu and x.")

;; This seems redundant; does it effectively combine allocate-hankel
;; and init-hankel?  In this case we don't really need, and shouldn't
;; export it.
(defmfun new-hankel (size nu xmax)
  "gsl_dht_new"
  ((size size) (nu :double) (xmax :double))
  :c-return :pointer
  :documentation			; FDL
  "Allocate a Discrete Hankel transform object of size
   size and initializes it for the given values of nu and
   xmax.")

(defmfun free-hankel (hankel)
  "gsl_dht_free"
  ((hankel :pointer))
  :c-return :void
  :export nil
  :index (letm hankel)
  :documentation			; FDL
  "Free the Hankel object.")

(defmfun apply-hankel (hankel array-in array-out)
  "gsl_dht_apply"
  ((hankel :pointer) ((gsl-array array-in) :pointer)
   ((gsl-array array-out) :pointer))
  :documentation			; FDL
  "Apply the transform to the array array-in
   whose size is equal to the size of the transform.  The result is stored
   in the array array-out which must be of the same length.")

(defmfun sample-x-hankel (hankel n)
  "gsl_dht_x_sample"
  ((hankel :pointer) (n :int))
  :c-return :double
  :documentation			; FDL
  "The points where the function f(t) is assumed to be sampled.")

(defmfun sample-k-hankel (hankel n)
  "gsl_dht_k_sample"
  ((hankel :pointer) (n :int))
  :c-return :double
  :documentation			; FDL
  "The value of the n-th sample point in k-space, j_{\nu,n+1}/X}.")
