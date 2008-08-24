;; Index lookup and acceleration
;; Liam Healy, Sun Nov  4 2007 - 18:09
;; Time-stamp: <2008-08-23 11:03:01EDT lookup.lisp>
;; $Id$

(in-package :gsl)

(defgo-s (acceleration) allocate-acceleration free-acceleration nil 0)

(defmfun interpolation-search (x-array x low-index high-index)
  "gsl_interp_bsearch"
  ((x-array :pointer) (x :double) (low-index sizet) (high-index sizet))
  :c-return sizet
  :documentation			; FDL
  "Find the index i of the array x-array such
   that x-array[i] <= x < x-array[i+1].  The index is searched for
   in the range [low-index, high-index].")

(defmfun allocate-acceleration ()
  "gsl_interp_accel_alloc"
  ()
  :c-return :pointer
  :export nil
  :index (letm acceleration)
  :documentation			; FDL
  "Allocate an accelerator object, which is a
   kind of iterator for interpolation lookups.  It tracks the state of
   lookups, thus allowing for application of various acceleration
   strategies.")

(defmfun accelerated-interpolation-search (x-array x acceleration)
  "gsl_interp_accel_find"
  ((acceleration :pointer) (x-array :pointer) (x :double))
  :c-return sizet
  :documentation			; FDL
  "Search the data array x-array of size, using the given acceleration.
   This is how lookups are performed during evaluation of an interpolation.  The
   function returns an index i such that x_array[i] <= x < x_array[i+1]}.")

(defmfun free-acceleration (acceleration)
  "gsl_interp_accel_free"
  ((acceleration :pointer))
  :export nil
  :index (letm acceleration)
  :c-return :void
  :documentation			; FDL
  "Frees the accelerator object.")
