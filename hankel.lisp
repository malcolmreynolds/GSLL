;********************************************************
; file:        hankel.lisp                               
; description: Discrete Hankel Transforms.               
; date:        Sat Dec  8 2007 - 16:50                   
; author:      Liam Healy                                
; modified:    Sat Dec  8 2007 - 18:38
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Everything compiles, but not tested -- need example.

(defun-gsl allocate-hankel (size)
  "gsl_dht_alloc"
  ((size :size))
  :c-return :pointer
  :documentation
  "Allocate a Discrete Hankel transform object of size @var{size}.")

(defun-gsl init-hankel (hankel nu xmax)
  "gsl_dht_new"
  ((hankel :pointer) (nu :double) (xmax :double))
  :documentation
  "Initialize the transform @var{t} for the given values of
   @var{nu} and @var{x}.")

(defun-gsl new-hankel (size nu xmax)
  "gsl_dht_new"
  ((size :size) (nu :double) (xmax :double))
  :c-return :pointer
  :documentation
  "Allocate a Discrete Hankel transform object of size
   @var{size} and initializes it for the given values of @var{nu} and
   @var{x}.")

(defun-gsl free-hankel (hankel)
  "gsl_dht_free"
  ((hankel :pointer))
  :c-return :void
  :documentation
  "Free the Hankel object.")

(defun-gsl apply-hankel (hankel array-in array-out)
  "gsl_dht_apply"
  ((hankel :pointer) ((gsl-array array-in) :pointer)
   ((gsl-array array-out) :pointer))
  :documentation
  "Apply the transform @var{t} to the array @var{f_in}
   whose size is equal to the size of the transform.  The result is stored
   in the array @var{f_out} which must be of the same length.")

(defun-gsl sample-x-hankel (hankel n)
  "gsl_dht_x_sample"
  ((hankel :pointer) (n :int))
  :c-return :double
  :documentation
  "The points where the function @math{f(t)} is assumed to be sampled.")

(defun-gsl sample-k-hankel (hankel n)
  "gsl_dht_k_sample"
  ((hankel :pointer) (n :int))
  :c-return :double
  :documentation
  "The value of the @var{n}-th sample point in k-space,
   j_{\nu,n+1}/X}.")
