;; Simulated Annealing
;; Liam Healy Sun Feb 11 2007 - 17:23
;; Time-stamp: <2009-05-16 23:18:31EDT simulated-annealing.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_siman.h

;;;;****************************************************************************
;;;; Simulated annealing argument structure
;;;;****************************************************************************

(fsbv:defcstruct simulated-annealing-parameters
  (n-tries :int)		; how many points to try for each step
  (iterations-fixed-T :int) ; how many iterations at each temperature?
  (step-size :double)		    ; max step size in the random walk
  ;; The following parameters are for the Boltzmann distribution
  (k :double)
  (t-initial :double)
  (mu-t :double)
  (t-min :double))

;;;;****************************************************************************
;;;; Callbacks
;;;;****************************************************************************

;;; A "state pointer" is in integer with a value of 0, 1, or 2.  The
;;; users functions must suitably define state and be prepared to keep
;;; three instances.  When they callbacks are called they will be
;;; directed to do certain operations on one (or two) of the three
;;; states.

(defun state-pointer (foreign-pointer)
  (cffi:pointer-address (cffi:mem-aref foreign-pointer :pointer)))

;;; The user-energy-function should take one state pointers, and
;;; return a double-float.
;;; typedef double (*gsl_siman_Efunc_t) (void *xp);
(cffi:defcallback sa-energy-function :double ((state :pointer))
  (declare (special user-energy-function))
  (funcall user-energy-function (state-pointer state)))

;;; The user-step-function should take a rng pointer, a state pointer,
;;; and a double-float, and return nothing.  The rng pointer should be
;;; used in call to one of the distribution functions like #'uniform.
;;; typedef void (*gsl_siman_step_t) (const gsl_rng *r, void *xp, double step_size);
(cffi:defcallback sa-step-function :void
    ((rng :pointer) (state :pointer) (step-size :double))
  (declare (special user-step-function))
  (funcall user-step-function rng (state-pointer state) step-size))

;;; typedef double (*gsl_siman_metric_t) (void *xp, void *yp);
(cffi:defcallback sa-metric-function :double
    ((state1 :pointer) (state2 :pointer))
  (declare (special user-metric-function))
  (funcall user-metric-function (state-pointer state1) (state-pointer state2)))

;;; typedef void (*gsl_siman_print_t) (void *xp);
(cffi:defcallback sa-print-function :void ((state :pointer))
  (declare (special user-print-function))
  (funcall user-print-function (state-pointer state)))

;;; The user-copy-function should take two state pointers, and return nothing.
;;; typedef void (*gsl_siman_copy_t) (void *source, void *dest);
(cffi:defcallback sa-copy-function :void ((source :pointer) (destination :pointer))
  (declare (special user-copy-function))
  (funcall user-copy-function
	   (state-pointer source)
	   (state-pointer destination)))

;;; The copy constructor will be special.  It will only be called
;;; three times in the run, and it will generate in succession the
;;; three pointer values.  It should expect copy state from the
;;; supplied value.
;;; typedef void * (*gsl_siman_copy_construct_t) (void *xp);
(cffi:defcallback sa-copy-constructor-function :pointer ((state :pointer))
  (declare (special sa-state-counter user-maker-function))
  (prog1
      (cffi:make-pointer sa-state-counter)
    (make-)
    (incf sa-state-counter)))

;;; Destructor?  How quaint.  Don't do anything.
;;; typedef void (*gsl_siman_destroy_t) (void *xp);
(cffi:defcallback sa-destroy-function :void ((state :pointer))
  (declare (ignore state))
  nil)

;;;;****************************************************************************
;;;; New
;;;;****************************************************************************

(defun simulated-annealing
    (parameters generator
     state-maker-function
     energy-function step-function metric-function
     print-function copy-function)
  (declare (special user-state-maker-function
		    user-energy-function user-step-function
		    user-metric-function user-print-function
		    user-copy-function sa-state-counter))
  (let ((sa-state-counter 0)
	(user-state-maker-function state-maker-function)
	(user-energy-function energy-function)
	(user-step-function step-function)
	(user-metric-function metric-function)
	(user-print-function print-function)
	(user-copy-function copy-function)
	(user-maker-function state-maker-function))
    (simulated-annealing-int
     parameters generator
     x0-p
     sa-energy-function
     sa-step-function
     sa-metric-function
     sa-print-function)))

(defmfun simulated-annealing-int
    (parameters generator x0-p
	       energy-function step-function metric-function
	        print-position)
  "gsl_siman_solve"
  (((mpointer generator) :pointer) (x0-p :pointer)
   ((cffi:get-callback energy-function) :pointer)
   ((cffi:get-callback step-function) :pointer)
   ((cffi:get-callback metric-function) :pointer)
   ((cffi:get-callback print-position) :pointer)
   ((cffi:get-callback 'sa-copy-function) :pointer)
   ((cffi:get-callback 'sa-copy-constructor-function) :pointer)
   ((cffi:get-callback 'sa-destroy-function) :pointer)
   (0 sizet)
   (parameters simulated-annealing-parameters))
  :c-return :void
  :documentation			; FDL
  "Perform a simulated annealing search through a given
   space.  The space is specified by providing the functions energy-function and
   distance.  The simulated annealing steps are generated using the
   random number generator r and the function step-function.

   The starting configuration of the system should be given by x0-p
   The routine offers two modes for updating configurations, a fixed-size
   mode and a variable-size mode.  In the fixed-size mode the configuration
   is stored as a single block of memory of size element-size
   The functions copy-function,
   copy-constructor and destructor should be NIL in
   fixed-size mode.  In the variable-size mode the functions
   copy-function, copy-constructor and destructor are used to
   create, copy and destroy configurations internally.  The variable
   element-size should be zero in the variable-size mode.

   The parameters structure (described below) controls the run by
   providing the temperature schedule and other tunable parameters to the
   algorithm.

   On exit the best result achieved during the search is placed in
   x0-p.  If the annealing process has been successful this
   should be a good approximation to the optimal point in the space.

   If the function pointer print-position is not null, a debugging
   log will be printed to standard output with the following columns:
   number_of_iterations temperature x x-x0p Ef(x)
   and the output of the function print-position itself.  If
   print-position is null then no information is printed.
   The simulated annealing routines require several user-specified
   functions to define the configuration space and energy function.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************

;;; Trivial example, Sec. 24.3.1
;;; This does not work.

(defun M2 (cx cy)
  (with-c-double (cx x)
    (with-c-double (cy y)
      (abs (- x y)))))

(defvar *sa-function-calls*)

(defun E2 (arg)
  (with-c-double (arg x)
    (incf *sa-function-calls*)
    (when (> *sa-function-calls* 100)
      (error "too much"))
    (* (exp (- (expt (1- x) 2))) (sin (* 8 x)))))

(defun S2 (generator parameters step-size)
  (with-c-double (parameters x)
    ;;(format t "~&~d ~d" x step-size)
    (let ((step-size 10.0d0))		; this is coming in wrong, so we fix it
      (let ((rand (uniform generator)))
	(setf x (+ x (- (* 2 rand step-size) step-size)))))))

(defparameter *sa-example-print* nil)

;;; Print functions are a problem because it is likely that the C
;;; stdout and the CL *standard-output* are not the same stream.
;;; Also, it seems to ignore that a function is supplied, and avoids
;;; printing anything.
(defun P2 (arg)
  (with-c-double (arg x)
    (when *sa-example-print*
      (format T "~&from P2: ~a" x))))

(def-energy-function E2)
(def-distance-function M2)
(def-step-function S2)
(def-print-function P2)

(defun simulated-annealing-example ()
  (let ((*sa-function-calls* 0))
    (rng-environment-setup)
    (cffi:with-foreign-object (initial :double)
      (setf (dcref initial) 15.5d0)
      (with-simulated-annealing-parameters
	  (params 200 10 10.0d0 1.0d0 0.002d0 1.005d0 2.0d-6)
	(simulated-annealing
	 (make-random-number-generator +mt19937+ 0) initial
	 'E2 'S2 'M2 'P2
	 (cffi:foreign-type-size :double)
	 params)))))

