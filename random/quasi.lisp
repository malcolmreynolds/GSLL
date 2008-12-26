;; Quasi-random sequences in arbitrary dimensions.
;; Liam Healy, Sun Jul 16 2006 - 15:54
;; Time-stamp: <2008-12-26 11:36:28EST quasi.lisp>
;; $Id$

(in-package :gsl)

(defmobject quasi-random-number-generator
    "gsl_qrng"
  ((rng-type :pointer) (dimension :uint))
  "quasi random number generator"		; FDL
  "Make and optionally initialize the generator q to its starting point.
   Note that quasi-random sequences do not use a seed and always produce
   the same set of values."
  "init"
  nil)

(defmfun qrng-get (generator return-vector)
  "gsl_qrng_get"
  (((generator generator) :pointer) ((c-pointer return-vector) :pointer))
  :outputs (return-vector)
  :return (return-vector)
  :documentation			; FDL
  "Store the next point from the sequence generator q
   in the array.  The space available for it must match the
   dimension of the generator.  The point will lie in the range
   0 < x_i < 1 for each x_i.")

(defmfun rng-name ((instance quasi-random-number-generator))
  "gsl_qrng_name" (((generator instance) :pointer))
  :definition :method
  :c-return :string)

(defmfun rng-state ((instance quasi-random-number-generator))
  "gsl_qrng_state" (((generator instance) :pointer))
  :c-return :pointer
  :definition :method
  :export nil
  :index gsl-random-state)

(defmfun rng-size ((instance quasi-random-number-generator))
  "gsl_qrng_size" (((generator instance) :pointer))
  :c-return sizet
  :definition :method
  :export nil
  :index gsl-random-state)

(defmfun copy
    ((destination quasi-random-number-generator)
     (source quasi-random-number-generator))
  "gsl_qrng_memcpy"
  (((generator destination) :pointer) ((generator source) :pointer))
  :definition :method
  :documentation			; FDL
  "Copy the quasi-random sequence generator src into the
   pre-existing generator dest, making dest into an exact copy
   of src.  The two generators must be of the same type.")

(defmfun clone ((instance quasi-random-number-generator))
  "gsl_qrng_clone" (((generator instance) :pointer))
  :definition :method
  :c-return (mptr :pointer)
  :return ((make-instance 'quasi-random-number-generator :mpointer mptr))
  :documentation			; FDL
  "Create a new generator which is an exact copy of the original.")

(def-rng-type *niederreiter2*
    ;; FDL
    "Described in Bratley, Fox, Niederreiter,
     ACM Trans. Model. Comp. Sim. 2, 195 (1992). It is
     valid up to 12 dimensions."
  "gsl_qrng_niederreiter_2")

(def-rng-type *sobol*
    ;; FDL
    "This generator uses the Sobol sequence described in Antonov, Saleev,
    USSR Comput. Maths. Math. Phys. 19, 252 (1980). It is valid up to
    40 dimensions."
  "gsl_qrng_sobol")

;;; Examples and unit test
(save-test quasi-random-number-generators
  ;; This example is given in the GSL documentation
  (let ((gen (make-quasi-random-number-generator 2 *sobol*))
	  (vec (make-marray 'double-float :dimensions 2)))
     (loop repeat 5
	   do (qrng-get gen vec)
	   append (coerce (cl-array vec) 'list))))
