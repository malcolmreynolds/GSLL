;; Simulated Annealing
;; Liam Healy Sun Feb 11 2007 - 17:23
;; Time-stamp: <2009-05-17 22:57:01EDT simulated-annealing.lisp>
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
;;;; Simulated annealing states
;;;;****************************************************************************

;;; States will be saved in an array of length 4, *sa-states*, a
;;; local special.  The "state pointer" is an index of type (integer 0 3). 

(defun state-pointer (foreign-pointer)
  (cffi:pointer-address (cffi:mem-aref foreign-pointer :pointer)))

(defun sa-state-value (foreign-pointer)
  (declare (special *sa-states*))
  (aref *sa-states* (state-pointer foreign-pointer)))

(defun make-sa-states (length)
  (declare (special *sa-states*))
  (setf *sa-states* (make-array length :adjustable t)))

(defun make-new-sa-state (&rest arguments)
  "Make a new simulated annealing state.
   Pass any arguments to user-state-maker-function."
  (declare
   (special user-state-maker-function sa-state-counter *sa-states*))
  (prog1 (cffi:make-pointer sa-state-counter)
    (when (>= sa-state-counter (length *sa-states*))
      (adjust-array *sa-states* (1+ sa-state-counter)))
    (setf (aref *sa-states* sa-state-counter)
	  (apply user-state-maker-function arguments))
    (incf sa-state-counter)))

;;; The user-copy-function should take two states, and return nothing.
(defun copy-sa-state (source destination)
  (declare (special user-copy-function))
  (funcall user-copy-function
	   (sa-state-value source)
	   (sa-state-value destination)))

;;;;****************************************************************************
;;;; Callbacks
;;;;****************************************************************************

;;; The user-energy-function should take one state pointers, and
;;; return a double-float.
;;; typedef double (*gsl_siman_Efunc_t) (void *xp);
(cffi:defcallback sa-energy-function :double ((state :pointer))
  (declare (special user-energy-function))
  (funcall user-energy-function (sa-state-value state)))

;;; The user-step-function should take a rng pointer, a state pointer,
;;; and a double-float, and return nothing.  The rng pointer should be
;;; used in call to one of the distribution functions like #'uniform.
;;; typedef void (*gsl_siman_step_t) (const gsl_rng *r, void *xp, double step_size);
(cffi:defcallback sa-step-function :void
    ((rng :pointer) (state :pointer) (step-size :double))
  (declare (special user-step-function))
  (funcall user-step-function rng (sa-state-value state) step-size))

;;; typedef double (*gsl_siman_metric_t) (void *xp, void *yp);
(cffi:defcallback sa-metric-function :double
    ((state1 :pointer) (state2 :pointer))
  (declare (special user-metric-function))
  (funcall user-metric-function (sa-state-value state1) (sa-state-value state2)))

;;; typedef void (*gsl_siman_print_t) (void *xp);
(cffi:defcallback sa-print-function :void ((state :pointer))
  (declare (special user-print-function))
  (funcall user-print-function (sa-state-value state)))

;;; typedef void (*gsl_siman_copy_t) (void *source, void *dest);
(cffi:defcallback sa-copy-function :void ((source :pointer) (destination :pointer))
  (copy-sa-state source destination))

;;; typedef void * (*gsl_siman_copy_construct_t) (void *xp);
(cffi:defcallback sa-copy-constructor-function :pointer ((state :pointer))
  (let ((new (make-new-sa-state)))
    (copy-sa-state state new)
    new))

;;; Destructor?  How quaint.  Don't do anything.
;;; typedef void (*gsl_siman_destroy_t) (void *xp);
(cffi:defcallback sa-destroy-function :void ((state :pointer))
  (declare (ignore state))
  nil)

;;;;****************************************************************************
;;;; New
;;;;****************************************************************************

(export 'simulated-annealing)
(defun simulated-annealing
    (state-values parameters
     generator
     state-maker-function energy-function
     step-function metric-function print-function copy-function)
  (let ((sa-state-counter 0)
	(user-state-maker-function state-maker-function)
	(user-energy-function energy-function)
	(user-step-function step-function)
	(user-metric-function metric-function)
	(user-print-function print-function)
	(user-copy-function copy-function))
    (declare (special
	      sa-state-counter
	      user-state-maker-function user-energy-function
	      user-step-function user-metric-function
	      user-print-function user-copy-function))
    (make-sa-states 4)
    (simulated-annealing-int
     parameters
     generator
     (make-new-sa-state state-values)	; x0-p
     'sa-energy-function
     'sa-step-function
     'sa-metric-function
     'sa-print-function)))

(defmfun simulated-annealing-int
    (parameters generator x0-p
		energy-function step-function metric-function
	        print-function)
  "gsl_siman_solve"
  (((mpointer generator) :pointer) (x0-p :pointer)
   ((cffi:get-callback energy-function) :pointer)
   ((cffi:get-callback step-function) :pointer)
   ((cffi:get-callback metric-function) :pointer)
   ((cffi:get-callback print-function) :pointer)
   ((cffi:get-callback 'sa-copy-function) :pointer)
   ((cffi:get-callback 'sa-copy-constructor-function) :pointer)
   ((cffi:get-callback 'sa-destroy-function) :pointer)
   (0 sizet)
   (parameters simulated-annealing-parameters))
  :c-return :void
  :export nil
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

   If the function pointer print-function is not null, a debugging
   log will be printed to standard output with the following columns:
   number_of_iterations temperature x x-x0p Ef(x)
   and the output of the function print-function itself.  If
   print-function is null then no information is printed.
   The simulated annealing routines require several user-specified
   functions to define the configuration space and energy function.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************

;;; Trivial example, Sec. 24.3.1
;;; This does not work.

(defun trivial-example-energy (state)
  (let ((x (aref state 0)))
    (* (exp (- (expt (1- x) 2))) (sin (* 8 x)))))

(defun trivial-example-step (generator state step-size)
  (let ((rand (uniform generator))
	(x (aref state 0)))
    (setf x (+ x (- (* 2 rand step-size) step-size)))))

(defun trivial-example-metric (state1 state2)
    (abs (- (aref state1 0) (aref state2 0))))

(defun trivial-example-print (state)
  (princ (aref state 0)))

(defun simulated-annealing-example ()
  (simulated-annealing
   (list 15.5d0)
   (list 200 10 10.0d0 1.0d0 0.002d0 1.005d0 2.0d-6) ; parameters
   (make-random-number-generator +mt19937+ 0)
   (lambda (initial)
     (make-marray 'double-float :dimensions 1 :initial-contents initial))
   'trivial-example-energy
   'trivial-example-step
   'trivial-example-metric
   'trivial-example-print
   'copy))
