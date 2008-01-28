;********************************************************
; file:        lookup.lisp                               
; description: Index lookup and acceleration             
; date:        Sun Nov  4 2007 - 18:09                   
; author:      Liam Healy                                
; modified:    Sat Nov 10 2007 - 21:16
;********************************************************
;;; $Id: $

(in-package :gsl)

(set-asf acceleration allocate-acceleration free-acceleration nil 0)

(defun-gsl interpolation-search (x-array x low-index high-index)
  "gsl_interp_bsearch"
  ((x-array :pointer) (x :double) (low-index :size) (high-index :size))
  :c-return :size
  :documentation
  "Find the index @math{i} of the array @var{x-array} such
   that @code{x-array[i] <= x < x-array[i+1]}.  The index is searched for
   in the range [low-index, high-index].")

(defun-gsl allocate-acceleration ()
  "gsl_interp_accel_alloc"
  ()
  :c-return :pointer
  :export nil
  :index (with-gsl-objects acceleration)
  :documentation
  "Allocate an accelerator object, which is a
   kind of iterator for interpolation lookups.  It tracks the state of
   lookups, thus allowing for application of various acceleration
   strategies.")

(defun-gsl accelerated-interpolation-search (x-array x acceleration)
  "gsl_interp_accel_find"
  ((acceleration :pointer) (x-array :pointer) (x :double))
  :c-return :size
  :documentation
  "Search the data array @var{x_array}
   of size @var{size}, using the given accelerator @var{a}.  This is how
   lookups are performed during evaluation of an interpolation.  The
   function returns an index @math{i} such that @code{x_array[i] <= x <
   x_array[i+1]}.")

(defun-gsl free-acceleration (acceleration)
  "gsl_interp_accel_free"
  ((acceleration :pointer))
  :export nil
  :index (with-gsl-objects acceleration)
  :c-return :void
  :documentation
  "Frees the accelerator object.")
