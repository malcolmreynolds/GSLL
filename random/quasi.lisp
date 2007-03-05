;********************************************************
; file:        quasi.lisp                                
; description: Quasi-random sequences in arbitrary dimensions.
; date:        Sun Jul 16 2006 - 15:54                   
; author:      Liam M. Healy                             
; modified:    Sat Mar  3 2007 - 19:55
;********************************************************
;;; $Id: $

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
  :documentation
  "Instatiate a random number generator of specified type.
   For example, create an instance of the Tausworthe
   generator: (rng-alloc *taus*).
   The generator is automatically initialized with the default seed,
   @code{gsl_rng_default_seed}.  This is zero by default but can be changed
   either directly or by using the environment variable @code{GSL_RNG_SEED}")

(defun-gsl free ((generator quasi-random-number-generator))
  "gsl_qrng_free" (((generator generator) :pointer))
  :type :method
  :c-return :void
  :after ((setf (generator generator) nil))
  :documentation "Free all the memory associated with the generator.")

(defun-gsl init (generator)
  "gsl_qrng_init" (((generator generator) :pointer))
  :c-return :void
  :documentation
  "Reinitializes the generator @var{q} to its starting point.
   Note that quasi-random sequences do not use a seed and always produce
   the same set of values.")

(defun-gsl qrng-get (generator return-vector)
  "gsl_qrng_get"
  (((generator generator) :pointer) ((gsl-array return-vector) :pointer))
  :documentation
  "Store the next point from the sequence generator @var{q}
   in the array @var{x}.  The space available for @var{x} must match the
   dimension of the generator.  The point @var{x} will lie in the range
   @math{0 < x_i < 1} for each @math{x_i}."
  :invalidate (return-vector))

(defun-gsl rng-name ((instance quasi-random-number-generator))
  "gsl_qrng_name" (((generator instance) :pointer))
  :type :method
  :c-return :string
  :documentation "The name of the random number generator.")

(defun-gsl rng-state ((instance quasi-random-number-generator))
  "gsl_qrng_state" (((generator instance) :pointer))
  :c-return :pointer
  :type :method
  :export nil
  :index gsl-random-state
  :documentation "A pointer to the state of generator.")

(defun-gsl rng-size ((instance quasi-random-number-generator))
  "gsl_qrng_size" (((generator instance) :pointer))
  :c-return :size
  :type :method
  :export nil
  :index gsl-random-state
  :documentation "The size of the generator.")

(defun-gsl copy
    ((destination quasi-random-number-generator)
     (source quasi-random-number-generator))
  "gsl_qrng_memcpy"
  (((generator destination) :pointer) ((generator source) :pointer))
  :type :method
  :documentation
  "Copy the quasi-random sequence generator @var{src} into the
   pre-existing generator @var{dest}, making @var{dest} into an exact copy
   of @var{src}.  The two generators must be of the same type.")

(defun-gsl clone-generator ((instance quasi-random-number-generator))
  "gsl_qrng_clone" (((generator instance) :pointer))
  :c-return :pointer
  :type :method
  :documentation
  "Create a new generator which is an exact copy of the original.
   Don't use; use #'make-random-number-generator, #'copy instead.")

(def-rng-type *niederreiter2*
    "Described in Bratley, Fox, Niederreiter,
     @cite{ACM Trans. Model. Comp. Sim.} 2, 195 (1992). It is
     valid up to 12 dimensions."
  "gsl_qrng_niederreiter_2")

(eval-when (:load-toplevel :execute)
 (setf *default-quasi-random-number-generator*
       (make-quasi-random-number-generator 2 *niederreiter2*)))

(def-rng-type *sobol*
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
