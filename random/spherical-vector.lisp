;; Spherical Vector distribution
;; Liam Healy, Sun Oct  22 2006
;; Time-stamp: <2008-10-25 18:17:49EDT spherical-vector.lisp>
;; $Id$

(in-package :gsl)

;;; No test for #'direction-Nd yet.

(defmfun direction-2d (generator)
  "gsl_ran_dir_2d"
  (((generator generator) :pointer) (x :double) (y :double))
  :c-return :void
  :documentation			; FDL
  "A random direction vector v = (x,y) in
   two dimensions.  The vector is normalized such that
   |v|^2 = x^2 + y^2 = 1.")

(defmfun direction-2d-trig-method (generator)
  "gsl_ran_dir_2d_trig_method"
  (((generator generator) :pointer) (x :double) (y :double))
  :c-return :void
  :documentation			; FDL
  "A random direction vector v = (x,y) in
   two dimensions.  The vector is normalized such that
   |v|^2 = x^2 + y^2 = 1.  Uses trigonometric functions.")

(defmfun direction-3d (generator)
  "gsl_ran_dir_3d"
  (((generator generator) :pointer) (x :double) (y :double) (z :double))
  :c-return :void
  :documentation			; FDL
  "A random direction vector v =
  (x,y,z) in three dimensions.  The vector is normalized
  such that |v|^2 = x^2 + y^2 + z^2 = 1.  The method employed is
  due to Robert E. Knop (CACM 13, 326 (1970)), and explained in Knuth, v2,
  3rd ed, p136.  It uses the surprising fact that the distribution
  projected along any axis is actually uniform (this is only true for 3
  dimensions).")

(defmfun direction-Nd (generator vector)
  "gsl_ran_dir_nd"
  (((generator generator) :pointer) ((dim0 vector) sizet)
   ((c-pointer vector) :pointer))
  :c-return :void
  :outputs (vector)
  :return (vector)
  :documentation			; FDL
  "A random direction vector v = (x_1,x_2,...,x_n) in n dimensions,
   where n is the length of the vector x passed in. The vector is normalized such that 
   |v|^2 = x_1^2 + x_2^2 + ... + x_n^2 = 1.  The method
   uses the fact that a multivariate gaussian distribution is spherically
   symmetric.  Each component is generated to have a gaussian distribution,
   and then the components are normalized.  The method is described by
   Knuth, v2, 3rd ed, p135--136, and attributed to G. W. Brown, Modern
   Mathematics for the Engineer (1956).")

;;; Examples and unit test
(save-test spherical-vector
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 4
	    append
	    (multiple-value-list (direction-2d rng))))
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 4
	    append
	    (multiple-value-list (direction-2d-trig-method rng))))
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 2
	    append
	    (multiple-value-list (direction-3d rng)))))
