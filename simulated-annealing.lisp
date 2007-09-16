;********************************************************
; file:        simulated-annealing.lisp                  
; description: Simulated Annealing                       
; date:        Sun Feb 11 2007 - 17:23                   
; author:      Liam Healy                                
; modified:    Sat Sep 15 2007 - 18:21
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; There are some problems.  First, step-size is not successfully
;;; returned by GSL to the step function, it comes back as garbage.
;;; That is not a big problem because it's never used by GSL, it is
;;; only sent back, so we can just define and use it locally.  The
;;; main problem is that it seems to run forever, even on the
;;; "trivial" example.  This is apparently because n-tries is
;;; not getting to GSL either.  It seems the problem is with
;;; simulated-annealing-parameters.
;;; Also, the code could use quite a bit of clean
;;; up to present a simpler interface.


(cffi:defcstruct simulated-annealing-parameters
  (n-tries :int)		; how many points to try for each step
  (iterations-fixed-T :int) ; how many iterations at each temperature?
  (step-size :double)		    ; max step size in the random walk
  ;; The following parameters are for the Boltzmann distribution
  (k :double)
  (t-initial :double)
  (mu-t :double)
  (t-min :double))

(defun-gsl simulated-annealing
    (generator x0-p
	       Ef take-step distance-function
	        print-position
		;; copy-function copy-constructor destructor
	       element-size parameters)
  "gsl_siman_solve"
  (((generator generator) :pointer) (x0-p :pointer)
   ((cffi:get-callback Ef) :pointer)
   ((cffi:get-callback take-step) :pointer)
   ((cffi:get-callback distance-function) :pointer)
   ((cffi:get-callback print-position) :pointer)
   ((cffi:null-pointer) :pointer)
   ((cffi:null-pointer) :pointer)
   ((cffi:null-pointer) :pointer)
   ;;((cffi:get-callback copy-function) :pointer)
   ;;((cffi:get-callback copy-constructor) :pointer)
   ;;((cffi:get-callback destructor) :pointer)
   (element-size :size) (parameters simulated-annealing-parameters))
  :c-return :void
  :documentation
  "Perform a simulated annealing search through a given
   space.  The space is specified by providing the functions @var{Ef} and
   @var{distance}.  The simulated annealing steps are generated using the
   random number generator @var{r} and the function @var{take_step}.

   The starting configuration of the system should be given by x0-p
   The routine offers two modes for updating configurations, a fixed-size
   mode and a variable-size mode.  In the fixed-size mode the configuration
   is stored as a single block of memory of size element-size
   The functions copy-function,
   copy-constructor and destructor should be NIL in
   fixed-size mode.  In the variable-size mode the functions
   copy-function, copy-constructor and destructor are used to
   create, copy and destroy configurations internally.  The variable
   @var{element_size} should be zero in the variable-size mode.

   The parameters structure (described below) controls the run by
   providing the temperature schedule and other tunable parameters to the
   algorithm.

   On exit the best result achieved during the search is placed in
   x0-p.  If the annealing process has been successful this
   should be a good approximation to the optimal point in the space.

   If the function pointer @var{print_position} is not null, a debugging
   log will be printed to @code{stdout} with the following columns:
   number_of_iterations temperature x x-x0p Ef(x)
   and the output of the function @var{print_position} itself.  If
   @var{print_position} is null then no information is printed.
   The simulated annealing routines require several user-specified
   functions to define the configuration space and energy function.")

;; cribbed from def-gsl-function; unify?
(export 'def-sa-function)
(defmacro def-sa-function (name arg &body body)
  "Define a GSL (C) function of either one argument of type
   double (if arg is a symbol), or a C array of doubles
   (if arg is a list), for GSL simulated annealing functions."
  (let ((argvec (gensym "MCARG")))
    `(cffi:defcallback ,name :double
      (,(if (listp arg)
	    `(,argvec :pointer)
	    `(,arg :double)))
      ,@(if (listp arg)
	    `((symbol-macrolet
		    ,(loop for i from 0 for a in arg
			   collect `(,a (cffi:mem-aref ,argvec :double ,i)))
		  ,@body))
	    body))))

(defparameter *sa-function-calls* 0)

(defcallback e1 :double ((xp :pointer))
  (incf *sa-function-calls*)
  (when (> *sa-function-calls* 100000000)
    (error "too much"))
  (let ((x (cffi:mem-aref xp :double)))
    (* (exp (- (expt (1- x) 2))) (sin (* 8 x)))))

(cffi:defcallback s1 :pointer
    ((generator :pointer) (parameters :pointer) (ss :double))
  (declare (ignore ss))
  (let ((step-size 10.0d0))
    (let ((rand (uniform generator)))
      (setf (cffi:mem-aref parameters :double)
	    (+ (cffi:mem-aref parameters :double)
	       (- (* 2 rand step-size) step-size))))
    (cffi:null-pointer)))

(def-sa-function M1 (x y) (abs (- x y)))

(defcallback P1 :void ((xp :pointer))
  (let ((x (cffi:mem-aref xp :double)))
    (FORMAT T "~&from P1: ~a" X)))

(defun simulated-annealing-example ()
  (cffi:with-foreign-object (initial :double)
    (setf (cffi:mem-aref initial :double) 15.0d0)
    (cffi:with-foreign-object (params 'simulated-annealing-parameters)
      (setf
       (cffi:foreign-slot-value
	params 'simulated-annealing-parameters 'n-tries)
       200
       (cffi:foreign-slot-value
	params 'simulated-annealing-parameters 'iterations-fixed-T)
       10
       (cffi:foreign-slot-value
	params 'simulated-annealing-parameters 'step-size)
       10.0d0
       ;; The following parameters are for the Boltzmann distribution
       (cffi:foreign-slot-value
	params 'simulated-annealing-parameters 'k)
       1.0d0
       (cffi:foreign-slot-value
	params 'simulated-annealing-parameters 't-initial)
       0.002d0
       (cffi:foreign-slot-value
	params 'simulated-annealing-parameters 'mu-t)
       1.005d0
       (cffi:foreign-slot-value
	params 'simulated-annealing-parameters 't-min)
       2.0d-6)
      (simulated-annealing
       *rng-mt19937* initial
       'E1 'S1 'M1 'P1
       (cffi:foreign-type-size :double)
       params))))

#|
@deftypefun void
gsl_siman_solve
(const gsl_rng * @var{r},
       void * @var{x0_p},
       gsl_siman_Efunc_t @var{Ef},
       gsl_siman_step_t @var{take_step},
       gsl_siman_metric_t @var{distance},
       gsl_siman_print_t @var{print_position},
       gsl_siman_copy_t @var{copyfunc},
       gsl_siman_copy_construct_t @var{copy_constructor},
       gsl_siman_destroy_t @var{destructor},
       size_t @var{element_size},
       gsl_siman_params_t @var{params})
|#

