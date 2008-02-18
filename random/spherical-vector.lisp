;; Spherical Vector distribution
;; Liam Healy, Sun Oct  22 2006
;; Time-stamp: <2008-02-17 13:25:44EST spherical-vector.lisp>
;; $Id: $

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

(defmfun direction-Nd (generator x)
  "gsl_ran_dir_nd"
  (((generator generator) :pointer) ((dim0 x) size) ((gsl-array x) :pointer))
  :c-return :void
  :return (x)
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
#|
(make-tests spherical-vector
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
|#

(LISP-UNIT:DEFINE-TEST SPHERICAL-VECTOR
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST -0.617745613497854d0 -0.7863779988047479d0
	  0.993748310886084d0 0.1116436053298841d0
	  -0.9458104280982743d0 0.3247193158722761d0
	  0.45726622946182216d0 0.8893298574734622d0
	  -0.46325616159849964d0 -0.8862244234622655d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 4 APPEND
	    (MULTIPLE-VALUE-LIST
	     (DIRECTION-2D RNG))))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.9999986835208556d0 -0.0016226387631051197d0
	  0.5203010106077766d0 0.8539829379797504d0
	  -0.2035120531038584d0 0.9790724407527016d0
	  0.9454753227485545d0 -0.3256937427607672d0
	  0.11500033916619544d0 0.9933654523848008d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 4 APPEND
	    (MULTIPLE-VALUE-LIST
	     (DIRECTION-2D-TRIG-METHOD RNG))))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST -0.09129925750445994d0 0.18782185357162273d0
	  0.977950610665004d0 -0.9051182961559773d0
	  -0.050683764485791594d0 -0.4221279734645046d0
	  0.13993766535985133d0 0.8385462620524484d0
	  -0.526552576872909d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 2 APPEND
	    (MULTIPLE-VALUE-LIST
	     (DIRECTION-3D RNG)))))))

