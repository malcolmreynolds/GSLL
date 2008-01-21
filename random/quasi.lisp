;; Quasi-random sequences in arbitrary dimensions.
;; Liam Healy, Sun Jul 16 2006 - 15:54
;; Time-stamp: <2008-01-21 11:39:10EST quasi.lisp>
;; $Id: $

(in-package :gsl)

(defclass quasi-random-number-generator (gsl-random)
  ((dimension :initarg :dimension :accessor qr-dimension))
  (:documentation "A generator of quasi-random numbers."))

(defparameter *default-quasi-random-number-generator* nil)

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

(defun-gsl alloc ((generator quasi-random-number-generator))
  "gsl_qrng_alloc"
  (((rng-type generator) :pointer) ((qr-dimension generator) :uint))
  :type :method
  :c-return (ptr :pointer)
  :return ((progn (setf (generator generator) ptr) generator))
  :documentation			; FDL 
  "Instatiate a random number generator of specified type.
   For example, create an instance of the Tausworthe
   generator: (rng-alloc *taus*).
   The generator is automatically initialized with the default seed,
   *default-seed*.  This is zero by default but can be changed
   either directly or by using the environment variable GSL_RNG_SEED.")

(defun-gsl free ((generator quasi-random-number-generator))
  "gsl_qrng_free" (((generator generator) :pointer))
  :type :method
  :c-return :void
  :after ((setf (generator generator) nil))
  :documentation			; FDL
  "Free all the memory associated with the generator.")

(defun-gsl init (generator)
  "gsl_qrng_init" (((generator generator) :pointer))
  :c-return :void
  :documentation			; FDL
  "Reinitializes the generator q to its starting point.
   Note that quasi-random sequences do not use a seed and always produce
   the same set of values.")

(defun-gsl qrng-get (generator return-vector)
  "gsl_qrng_get"
  (((generator generator) :pointer) ((gsl-array return-vector) :pointer))
  :documentation			; FDL
  "Store the next point from the sequence generator q
   in the array x.  The space available for x must match the
   dimension of the generator.  The point x will lie in the range
   0 < x_i < 1 for each x_i."
  :invalidate (return-vector))

(defun-gsl rng-name ((instance quasi-random-number-generator))
  "gsl_qrng_name" (((generator instance) :pointer))
  :type :method
  :c-return :string)

(defun-gsl rng-state ((instance quasi-random-number-generator))
  "gsl_qrng_state" (((generator instance) :pointer))
  :c-return :pointer
  :type :method
  :export nil
  :index gsl-random-state)

(defun-gsl rng-size ((instance quasi-random-number-generator))
  "gsl_qrng_size" (((generator instance) :pointer))
  :c-return :size
  :type :method
  :export nil
  :index gsl-random-state)

(defun-gsl copy
    ((destination quasi-random-number-generator)
     (source quasi-random-number-generator))
  "gsl_qrng_memcpy"
  (((generator destination) :pointer) ((generator source) :pointer))
  :type :method
  :documentation			; FDL
  "Copy the quasi-random sequence generator src into the
   pre-existing generator dest, making dest into an exact copy
   of src.  The two generators must be of the same type.")

(defun-gsl clone-generator ((instance quasi-random-number-generator))
  "gsl_qrng_clone" (((generator instance) :pointer))
  :c-return :pointer
  :type :method
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
    @cite{USSR Comput. Maths. Math. Phys.} 19, 252 (1980). It is valid up to
    40 dimensions."
  "gsl_qrng_sobol")

;;; Examples and unit test
(lisp-unit:define-test quasi-random-number-generators
  ;; This example is given in the GSL documentation
  (lisp-unit:assert-equal
   '(0.5d0 0.5d0 0.75d0 0.25d0 0.25d0 0.75d0 0.375d0 0.375d0 0.875d0 0.875d0)
   (with-data (vec vector-double 2)
     (let ((gen (make-quasi-random-number-generator 2 *sobol*)))
       (init gen)
       (prog1
	   (loop repeat 5
		 do (qrng-get gen vec)
		 append (coerce (data vec) 'list))
	 (free gen))))))
