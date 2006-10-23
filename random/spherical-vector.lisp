;********************************************************
; file:        spherical-vector.lisp                          
; description: Spherical Vector distribution                  
; date:        Sun Oct  22 2006
; author:      Liam M. Healy                             
; modified:    Sun Oct 22 2006 - 21:58
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; No test for #'direction-Nd yet.

(defun-gsl direction-2d (generator)
  "gsl_ran_dir_2d"
  (((generator generator) :pointer) (x :double) (y :double))
  :c-return :void
  :documentation
  "A random direction vector @math{v} = (@var{x},@var{y}) in
   two dimensions.  The vector is normalized such that
   @math{|v|^2 = x^2 + y^2 = 1}.")

(defun-gsl direction-2d-trig-method (generator)
  "gsl_ran_dir_2d_trig_method"
  (((generator generator) :pointer) (x :double) (y :double))
  :c-return :void
  :documentation
  "A random direction vector @math{v} = (@var{x},@var{y}) in
   two dimensions.  The vector is normalized such that
   @math{|v|^2 = x^2 + y^2 = 1}.  Uses trigonometric functions.")

(defun-gsl direction-3d (generator)
  "gsl_ran_dir_3d"
  (((generator generator) :pointer) (x :double) (y :double) (z :double))
  :c-return :void
  :documentation
  "A random direction vector @math{v} =
  (@var{x},@var{y},@var{z}) in three dimensions.  The vector is normalized
  such that @math{|v|^2 = x^2 + y^2 + z^2 = 1}.  The method employed is
  due to Robert E. Knop (CACM 13, 326 (1970)), and explained in Knuth, v2,
  3rd ed, p136.  It uses the surprising fact that the distribution
  projected along any axis is actually uniform (this is only true for 3
  dimensions).")

(defun-gsl direction-Nd (generator x)
  "gsl_ran_dir_nd"
  (((generator generator) :pointer) ((dim0 x) :size) ((gsl-array x) :pointer))
  :c-return :void
  :return (x)
  :documentation
  "A random direction vector @math{v = (x_1,x_2,...,x_n)} in @var{n} dimensions,
   where n is the length of the vector x passed in. The vector is normalized such that 
   @math{|v|^2 = x_1^2 + x_2^2 + ... + x_n^2 = 1}.  The method
   uses the fact that a multivariate gaussian distribution is spherically
   symmetric.  Each component is generated to have a gaussian distribution,
   and then the components are normalized.  The method is described by
   Knuth, v2, 3rd ed, p135--136, and attributed to G. W. Brown, Modern
   Mathematics for the Engineer (1956).")

;;; Examples and unit test
(lisp-unit:define-test spherical-vector
  (lisp-unit:assert-equal
   '("-0.617745613498d+00" "-0.786377998805d+00" "0.993748310886d+00"
     "0.111643605330d+00" "-0.945810428098d+00" "0.324719315872d+00"
     "0.457266229462d+00" "0.889329857473d+00" "-0.463256161598d+00"
     "-0.886224423462d+00")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 4
	    append
	    (multiple-value-list (direction-2d *rng-mt19937*))))))
  (lisp-unit:assert-equal
   '("0.999998683521d+00" "-0.162263876311d-02" "0.520301010608d+00"
     "0.853982937980d+00" "-0.203512053104d+00" "0.979072440753d+00"
     "0.945475322749d+00" "-0.325693742761d+00" "0.115000339166d+00"
     "0.993365452385d+00")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 4
	    append
	    (multiple-value-list (direction-2d-trig-method *rng-mt19937*))))))
  (lisp-unit:assert-equal
   '("-0.912992575045d-01" "0.187821853572d+00" "0.977950610665d+00"
     "-0.905118296156d+00" "-0.506837644858d-01" "-0.422127973465d+00"
     "0.139937665360d+00" "0.838546262052d+00" "-0.526552576873d+00")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 2
	    append
	    (multiple-value-list (direction-3d *rng-mt19937*)))))))
