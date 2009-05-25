;; Quasi-random sequences in arbitrary dimensions.
;; Liam Healy, Sun Jul 16 2006 - 15:54
;; Time-stamp: <2009-05-25 09:44:53EDT quasi.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_qrng.h

(defmobject quasi-random-number-generator
    "gsl_qrng"
  ((rng-type :pointer) (dimension :uint))
  "quasi random number generator"
  :documentation			; FDL
  "Make and optionally initialize the generator q to its starting point.
   Note that quasi-random sequences do not use a seed and always produce
   the same set of values."
  :initialize-suffix ("init" :void)
  :initialize-args nil)

(defmfun qrng-get (generator return-vector)
  "gsl_qrng_get"
  (((mpointer generator) :pointer) ((c-pointer return-vector) :pointer))
  :outputs (return-vector)
  :return (return-vector)
  :documentation			; FDL
  "Store the next point from the sequence generator q
   in the array.  The space available for it must match the
   dimension of the generator.  The point will lie in the range
   0 < x_i < 1 for each x_i.")

(defmfun name ((instance quasi-random-number-generator))
  "gsl_qrng_name" (((mpointer instance) :pointer))
  :definition :method
  :c-return :string)

(defmfun rng-state ((instance quasi-random-number-generator))
  "gsl_qrng_state" (((mpointer instance) :pointer))
  :definition :method
  :c-return :pointer
  :export nil
  :index gsl-random-state)

(defmfun size ((instance quasi-random-number-generator))
  "gsl_qrng_size" (((mpointer instance) :pointer))
  :definition :method
  :c-return sizet
  :export nil
  :index gsl-random-state)

(defmfun copy-to-destination
    ((source quasi-random-number-generator)
     (destination quasi-random-number-generator))
  "gsl_qrng_memcpy"
  (((mpointer destination) :pointer) ((mpointer source) :pointer))
  :definition :method
  :index copy
  :documentation			; FDL
  "Copy the quasi-random sequence generator src into the
   pre-existing generator dest, making dest into an exact copy
   of src.  The two generators must be of the same type.")

(defmfun copy-making-destination ((instance quasi-random-number-generator))
  "gsl_qrng_clone" (((mpointer instance) :pointer))
  :definition :method
  :c-return :pointer
  :index copy)

(def-rng-type +niederreiter2+
    ;; FDL
    "Described in Bratley, Fox, Niederreiter,
     ACM Trans. Model. Comp. Sim. 2, 195 (1992). It is
     valid up to 12 dimensions."
  "gsl_qrng_niederreiter_2")

(def-rng-type +sobol+
    ;; FDL
    "This generator uses the Sobol sequence described in Antonov, Saleev,
    USSR Comput. Maths. Math. Phys. 19, 252 (1980). It is valid up to
    40 dimensions."
  "gsl_qrng_sobol")

(def-rng-type +halton+ 
    ;; FDL
    "The Halton sequence described in J.H. Halton, Numerische
     Mathematik 2, 84-90 (1960) and B. Vandewoestyne and R. Cools
     Computational and Applied Mathematics 189, 1&2, 341-361 (2006). They
     are valid up to 1229 dimensions. "
  "gsl_qrng_halton"
  (1 11))

(def-rng-type +reverse-halton+ 
    ;; FDL
    "The reverse Halton sequence described in J.H. Halton, Numerische
     Mathematik 2, 84-90 (1960) and B. Vandewoestyne and R. Cools
     Computational and Applied Mathematics 189, 1&2, 341-361 (2006). They
     are valid up to 1229 dimensions. "
  "gsl_qrng_reversehalton"
  (1 11))

;;; Examples and unit test
(save-test quasi-random-number-generators
  ;; This example is given in the GSL documentation
  (let ((gen (make-quasi-random-number-generator +sobol+ 2))
	  (vec (make-marray 'double-float :dimensions 2)))
     (loop repeat 5
	   do (qrng-get gen vec)
	   append (coerce (cl-array vec) 'list))))
