;; Quasi-random sequences in arbitrary dimensions.
;; Liam Healy, Sun Jul 16 2006 - 15:54
;; Time-stamp: <2008-09-14 22:01:42EDT quasi.lisp>
;; $Id$

(in-package :gsl)

(defclass quasi-random-number-generator (gsl-random)
  ((dimension :initarg :dimension :accessor qr-dimension))
  (:documentation "A generator of quasi-random numbers."))

(defparameter *default-quasi-random-number-generator* nil)

(defgo-s (quasi-random-number-generator dimension type)
	 make-quasi-random-number-generator free nil 2)

(defun make-quasi-random-number-generator 
    (dimension
     &optional (type *default-quasi-random-number-generator*)
     (generator t))
  "Make a random number generator; by default it is allocated on creation."
  (let ((instance
	 (make-instance
	  'quasi-random-number-generator
	  :type type
	  :dimension dimension
	  :generator generator)))
    (if (eq generator t) (alloc instance) instance)))

(defmfun alloc ((generator quasi-random-number-generator))
  "gsl_qrng_alloc"
  (((rng-type generator) :pointer) ((qr-dimension generator) :uint))
  :definition :method
  :c-return (ptr :pointer)
  :return ((progn (setf (generator generator) ptr) generator))
  :documentation			; FDL 
  "Instatiate a random number generator of specified type.
   For example, create an instance of the Tausworthe
   generator: (rng-alloc *taus*).
   The generator is automatically initialized with the default seed,
   *default-seed*.  This is zero by default but can be changed
   either directly or by using the environment variable GSL_RNG_SEED.")

(defmfun free ((generator quasi-random-number-generator))
  "gsl_qrng_free" (((generator generator) :pointer))
  :definition :method
  :c-return :void
  :after ((setf (generator generator) nil))
  :documentation			; FDL
  "Free all the memory associated with the generator.")

(defmfun init (generator)
  "gsl_qrng_init" (((generator generator) :pointer))
  :c-return :void
  :documentation			; FDL
  "Reinitializes the generator q to its starting point.
   Note that quasi-random sequences do not use a seed and always produce
   the same set of values.")

(defmfun qrng-get (generator return-vector)
  "gsl_qrng_get"
  (((generator generator) :pointer) ((c-pointer return-vector) :pointer))
  :outputs (return-vector)
  :return (return-vector)
  :documentation			; FDL
  "Store the next point from the sequence generator q
   in the array.  The space available fopr it must match the
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

(defmfun clone-generator ((instance quasi-random-number-generator))
  "gsl_qrng_clone" (((generator instance) :pointer))
  :c-return :pointer
  :definition :method
  :documentation			; FDL
  "Create a new generator which is an exact copy of the original.
   Don't use; use #'make-random-number-generator, #'copy instead.")

(def-rng-type *niederreiter2*
    ;; FDL
    "Described in Bratley, Fox, Niederreiter,
     ACM Trans. Model. Comp. Sim. 2, 195 (1992). It is
     valid up to 12 dimensions."
  "gsl_qrng_niederreiter_2")

(eval-when (:load-toplevel :execute)
 (setf *default-quasi-random-number-generator*
       (make-quasi-random-number-generator 2 *niederreiter2*)))

(def-rng-type *sobol*
    ;; FDL
    "This generator uses the Sobol sequence described in Antonov, Saleev,
    USSR Comput. Maths. Math. Phys. 19, 252 (1980). It is valid up to
    40 dimensions."
  "gsl_qrng_sobol")

;;; Examples and unit test
#|
(make-tests quasi-random-number-generators
  ;; This example is given in the GSL documentation
  (letm ((gen (quasi-random-number-generator 2 *sobol*))
	  (vec (vector-double-float 2)))
     (loop repeat 5
	   do (qrng-get gen vec)
	   append (coerce (cl-array vec) 'list))))
|#

(LISP-UNIT:DEFINE-TEST QUASI-RANDOM-NUMBER-GENERATORS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.5d0 0.5d0 0.75d0 0.25d0 0.25d0 0.75d0 0.375d0
	  0.375d0 0.875d0 0.875d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((GEN (QUASI-RANDOM-NUMBER-GENERATOR 2 *SOBOL*))
	 (VEC (VECTOR-DOUBLE-FLOAT 2)))
      (LOOP REPEAT 5 DO (QRNG-GET GEN VEC) APPEND
	    (COERCE (CL-ARRAY VEC) 'LIST))))))

