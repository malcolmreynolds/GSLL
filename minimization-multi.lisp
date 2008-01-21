;; Multivariate minimization.
;; Liam Healy  <Tue Jan  8 2008 - 21:28>
;; Time-stamp: <2008-01-20 21:01:12EST minimization-multi.lisp>
;; $Id: $

(in-package :gsl)

;; In the parabaloid example, I notice that the consruct 
;; (min-test-gradient (mfdfminimizer-gradient minimizer) 1.0d-3)
;; is constructing a CL vector-double (in mfdfminimizer-gradient) and
;; then immediately pulling out the pointer (in min-test-gradient).  It
;; is easy enough to eliminate this, but then mfdfminimizer-gradient
;; would not be useful to a CL user.

;;;;****************************************************************************
;;;; Function definition
;;;;****************************************************************************

;;; The structures gsl-mfunction and gsl-mfunction-fdf are, from the
;;; CFFI point of view, equally valid for gsl_multimin_function and
;;; gsl_multimin_function_fdf defined in
;;; /usr/include/gsl/gsl_multimin.h as they are for
;;; gsl_multiroot_function and gsl_multiroot_function_fdf defined in
;;; /usr/include/gsl/gsl_multiroots.h.  As far as CFFI is concerned, a
;;; pointer is a pointer, even though the C definition the functions
;;; they point to have different signatures.

(export 'def-minimization-functions)
(defmacro def-minimization-functions (function dimensions &optional df fdf)
  "Setup functions for multivariate minimization.
   The CL functions name and derivative should be defined previously
   with defuns."
  `(progn
    (defmcallback ,function :double :pointer)
    ,@(when df
	    `((defmcallback ,df :pointer :pointer :pointer)
	      (defmcallback ,fdf :pointer :pointer (:pointer :pointer))))
    ,(if df
	 `(defcbstruct (,function function ,df df ,fdf fdf)
	   gsl-mfunction-fdf
	   ((dimensions ,dimensions)))
	 `(defcbstruct (,function function)
	   gsl-mfunction
	   ((dimensions ,dimensions))))))


;;;;****************************************************************************
;;;; Initialization
;;;;****************************************************************************

(defun-gsl allocate-mfminimizer (type dimension)
  "gsl_multimin_fminimizer_alloc"
  ((type :pointer) (dimension :size))
  :c-return :pointer
  :documentation			; FDL
  "Allocate an instance of a minimizer of the given for an
   function of the given dimensions.")

(defun-gsl allocate-mfdfminimizer (type dimension)
  "gsl_multimin_fdfminimizer_alloc"
  ((type :pointer) (dimension :size))
  :c-return :pointer
  :documentation			; FDL
  "Allocate an instance of a derivative-based minimizer of the given for an
   function of the given dimensions.")

(defun-gsl set-mfminimizer (minimizer function initial step-size)
  "gsl_multimin_fminimizer_set"
  ((minimizer :pointer) (function :pointer)
   ((pointer initial) :pointer) ((pointer step-size) :pointer))
  :documentation			; FDL
  "Initialize the minimizer to minimize the function
   starting from the initial point.
   The size of the initial trial steps is given in vector
   step-size. The precise meaning of this parameter depends on the
   method used.")

(defun-gsl set-mfdfminimizer
    (minimizer function-derivative initial step-size tolerance)
  "gsl_multimin_fdfminimizer_set"
  ((minimizer :pointer) (function-derivative :pointer)
   ((pointer initial) :pointer) (step-size :double)
   (tolerance :double))
  :documentation			; FDL
  "Initialize the minimizer to minimize the function
   starting from the initial point.  The size of the
   first trial step is given by step-size.  The accuracy of the line
   minimization is specified by tolernace.  The precise meaning of this
   parameter depends on the method used.  Typically the line minimization
   is considered successful if the gradient of the function g is
   orthogonal to the current search direction p to a relative
   accuracy of tolerance, where dot(p,g) < tol |p| |g|.")

(defun-gsl free-mfminimizer (minimizer)
  "gsl_multimin_fminimizer_free"
  ((minimizer :pointer))
  :c-return :void
  :documentation			; FDL
  "Free all the memory associated with the minimizer.")

(defun-gsl free-mfdfminimizer (minimizer)
  "gsl_multimin_fdfminimizer_free"
  ((minimizer :pointer))
  :c-return :void
  :documentation			; FDL
  "Free all the memory associated with the minimizer.")

(defun-gsl mfminimizer-name (minimizer)
  "gsl_multimin_fminimizer_name"
  ((minimizer :pointer))
  :c-return :string
  :documentation
  "The name of the minimizer.")

(defun-gsl mfdfminimizer-name (minimizer)
  "gsl_multimin_fdfminimizer_name"
  ((minimizer :pointer))
  :c-return :string
  :documentation
  "The name of the minimizer.")

(export '(with-mfdfminimizer with-mfminimizer))
(defmacro with-mfminimizer
    ((minimizer minimizer-type function initial step-size dimensions)
     &body body)
  "Create and initialize an fdfminimizer for multi-dimensional problems,
   and clean up afterwards."
  `(let ((,minimizer (allocate-mfminimizer ,minimizer-type ,dimensions)))
    (unwind-protect
	 (progn
	   (set-mfminimizer ,minimizer ,function ,initial ,step-size)
	   ,@body)
      (free-mfminimizer ,minimizer))))

(defmacro with-mfdfminimizer
    ((minimizer minimizer-type fdf initial step-size tolerance dimensions)
     &body body)
  "Create and initialize an fdfminimizer for multi-dimensional problems,
   and clean up afterwards."
  `(let ((,minimizer (allocate-mfdfminimizer ,minimizer-type ,dimensions)))
    (unwind-protect
	 (progn
	   (set-mfdfminimizer ,minimizer ,fdf ,initial ,step-size ,tolerance)
	   ,@body)
      (free-mfdfminimizer ,minimizer))))

;;;;****************************************************************************
;;;; Iteration
;;;;****************************************************************************

(defun-gsl iterate-mfminimizer (minimizer)
  "gsl_multimin_fminimizer_iterate"
  ((minimizer :pointer))
  :documentation			; FDL
  "Perform a single iteration of the minimizer.  If the iteration
   encounters an unexpected problem then an error code will be
   returned.")

(defun-gsl iterate-mfdfminimizer (minimizer)
  "gsl_multimin_fdfminimizer_iterate"
  ((minimizer :pointer))
  :documentation			; FDL
  "Perform a single iteration of the minimizer.  If the iteration
   encounters an unexpected problem then an error code will be
   returned.")

(defun-gsl mfminimizer-x (solver)
  "gsl_multimin_fminimizer_x"
  ((solver :pointer))
  :c-return (canswer :pointer)
  :return ((make-data-from-pointer canswer))
  :documentation			; FDL
  "The current best estimate of the location of the minimum.")

(defun-gsl mfdfminimizer-x (solver)
  "gsl_multimin_fdfminimizer_x"
  ((solver :pointer))
  :c-return (canswer :pointer)
  :return ((make-data-from-pointer canswer))
  :documentation			; FDL
  "The current best estimate of the location of the minimum.")

(defun-gsl mfminimizer-minimum (solver)
  "gsl_multimin_fminimizer_minimum"
  ((solver :pointer))
  :c-return :double
  :documentation			; FDL
  "The current best estimate of the value of the minimum.")

(defun-gsl mfdfminimizer-minimum (solver)
  "gsl_multimin_fdfminimizer_minimum"
  ((solver :pointer))
  :c-return :double
  :documentation			; FDL
  "The current best estimate of the value of the minimum.")

(defun-gsl mfminimizer-size (solver)
  "gsl_multimin_fminimizer_size"
  ((solver :pointer))
  :c-return :double
  :documentation			; FDL
  "A minimizer-specific characteristic size for the minimizer.")

(defun-gsl mfdfminimizer-gradient (solver)
  "gsl_multimin_fdfminimizer_gradient"
  ((solver :pointer))
  :c-return (canswer :pointer)
  :return ((make-data-from-pointer canswer))
  :documentation			; FDL
  "The current best estimate of the gradient for the minimizer.")

(defun-gsl mfdfminimizer-restart (solver)
  "gsl_multimin_fdfminimizer_restart"
  ((solver :pointer))
  :documentation			; FDL
  "Reset the minimizer to use the current point as a
   new starting point.")

;;;;****************************************************************************
;;;; Stopping criteria
;;;;****************************************************************************

(defun-gsl min-test-gradient (gradient absolute-error)
  "gsl_multimin_test_gradient"
  (((pointer gradient) :pointer) (absolute-error :double))
  :c-return :success-continue
  :documentation			; FDL
  "Test the norm of the gradient against the
   absolute tolerance absolute-error.  The gradient of a multidimensional
   function goes to zero at a minimum. The test returns T
   if |g| < epsabs is achieved, and NIL otherwise.  A suitable choice of
   absolute-error can be made from the desired accuracy in the function for
   small variations in x.  The relationship between these quantities
   \delta f = g \delta x.")

(defun-gsl min-test-size (size absolute-error)
  "gsl_multimin_test_size"
  ((size :double) (absolute-error :double))
  :c-return :success-continue
  :documentation			; FDL
  "Test the minimizer specific characteristic size (if applicable to
   the used minimizer) against absolute tolerance @var{epsabs}.  The test
   returns T if the size is smaller than tolerance, and NIL otherwise.")

;;;;****************************************************************************
;;;; Algorithms
;;;;****************************************************************************

(defvariable *conjugate-fletcher-reeves*
    "gsl_multimin_fdfminimizer_conjugate_fr"
  ;; FDL
  "The Fletcher-Reeves conjugate gradient algorithm. The conjugate
   gradient algorithm proceeds as a succession of line minimizations. The
   sequence of search directions is used to build up an approximation to the
   curvature of the function in the neighborhood of the minimum.  

   An initial search direction p is chosen using the gradient, and
   line minimization is carried out in that direction.  The accuracy of
   the line minimization is specified by the parameter tol.  The minimum
   along this line occurs when the function gradient g and the search
   direction p are orthogonal.  The line minimization terminates when
   dot(p,g) < tol |p| |g|.  The search direction is updated using the
   Fletcher-Reeves formula p' = g' - \beta g where \beta=-|g'|^2/|g|^2,
   and the line minimization is then repeated for the new search
   direction.")

(defvariable *conjugate-polak-ribiere*
    "gsl_multimin_fdfminimizer_conjugate_pr"
  ;; FDL
  "The Polak-Ribiere conjugate gradient algorithm.  It is similar to
   the Fletcher-Reeves method, differing only in the choice of the
   coefficient \beta. Both methods work well when the evaluation point is
   close enough to the minimum of the objective function that it is well
   approximated by a quadratic hypersurface.")

(defvariable *vector-bfgs*
    "gsl_multimin_fdfminimizer_vector_bfgs"
  ;; FDL
  "The vector Broyden-Fletcher-Goldfarb-Shanno (BFGS) conjugate
   gradient algorithm.  It is a quasi-Newton method which builds up an
   approximation to the second derivatives of the function using the
   difference between successive gradient vectors.  By combining the
   first and second derivatives the algorithm is able to take Newton-type
   steps towards the function minimum, assuming quadratic behavior in
   that region.")

(defvariable *simplex-nelder-mead*
    "gsl_multimin_fminimizer_nmsimplex"
  ;; FDL
  "The Simplex algorithm of Nelder and Mead. It constructs 
   n vectors p_i from the
   starting vector initial and the vector step-size as follows:
   p_0 = (x_0, x_1, ... , x_n) 
   p_1 = (x_0 + step_size_0, x_1, ... , x_n) 
   p_2 = (x_0, x_1 + step_size_1, ... , x_n) 
   ... = ...
   p_n = (x_0, x_1, ... , x_n+step_size_n)
   These vectors form the n+1 vertices of a simplex in n
   dimensions.  On each iteration the algorithm tries to improve
   the parameter vector p_i corresponding to the highest
   function value by simple geometrical transformations.  These
   are reflection, reflection followed by expansion, contraction and multiple
   contraction. Using these transformations the simplex moves through 
   the parameter space towards the minimum, where it contracts itself.  

   After each iteration, the best vertex is returned.  Note, that due to
   the nature of the algorithm not every step improves the current
   best parameter vector.  Usually several iterations are required.

   The routine calculates the minimizer specific characteristic size as the
   average distance from the geometrical center of the simplex to all its
   vertices.  This size can be used as a stopping criteria, as the simplex
   contracts itself near the minimum. The size is returned by the function
   #'mfminimizer-size.")

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

;;; Examples from Sec. 35.8.

(defparameter *parabaloid-center* #(1.0d0 2.0d0))

(defun parabaloid (gsl-vector-pointer)
  "A parabaloid function of two arguments, given in GSL manual Sec. 35.4."
  (let ((x (vref gsl-vector-pointer 0))
	(y (vref gsl-vector-pointer 1))
	(dp0 (aref *parabaloid-center* 0))
	(dp1 (aref *parabaloid-center* 1)))
    (+ (* 10 (expt (- x dp0) 2))
       (* 20 (expt (- y dp1) 2))
       30)))

(defun parabaloid-derivative
    (arguments-gv-pointer derivative-gv-pointer)
  (let ((x (vref arguments-gv-pointer 0))
	(y (vref arguments-gv-pointer 1))
	(dp0 (aref *parabaloid-center* 0))
	(dp1 (aref *parabaloid-center* 1)))
    (setf (vref derivative-gv-pointer 0)
	  (* 20 (- x dp0))
	  (vref derivative-gv-pointer 1)
	  (* 40 (- y dp1)))))

(defun parabaloid-and-derivative
    (arguments-gv-pointer fnval derivative-gv-pointer)
  (with-c-double (function fnval)
    (setf fnval (parabaloid arguments-gv-pointer))
    (parabaloid-derivative
     arguments-gv-pointer derivative-gv-pointer))))

(def-minimization-functions
    parabaloid 2 parabaloid-derivative parabaloid-and-derivative)

(defun multimin-example-fletcher-reeves ()
  (with-data (initial vector-double 2)
    (setf (data initial) #(5.0d0 7.0d0))
    (with-mfdfminimizer
	(minimizer *conjugate-fletcher-reeves* parabaloid
		   initial 0.01d0 1.0d-4 2)
      (loop with status = T
	    for iter from 0 below 100
	    while status
	    do (iterate-mfdfminimizer minimizer)
	    (setf status
		  (not (min-test-gradient
			(mfdfminimizer-gradient minimizer)
			1.0d-3)))
	    (let ((x (mfdfminimizer-x minimizer)))
	      (format t "~&~d~6t~10,6f~18t~10,6f~28t~12,9f"
		      iter (gsl-aref x 0) (gsl-aref x 1)
		      (mfdfminimizer-minimum minimizer)))
	    finally (return
		      (let ((x (mfdfminimizer-x minimizer)))
			(values (gsl-aref x 0) (gsl-aref x 1))))))))

;;; Because def-minimization-functions bind a symbol
;;; of the same name as the first function, and we want both to run,
;;; we'll make an alias function so we can use both.  
(eval-when (:load-toplevel :execute)
  (setf (fdefinition 'parabaloid-f) #'parabaloid))

(def-minimization-functions parabaloid-f 2)

(defun multimin-example-nelder-mead ()
  (with-data (initial vector-double 2)
    (with-data (step-size vector-double 2)
      (setf (data initial) #(5.0d0 7.0d0))
      (set-all step-size 1.0d0)
      (with-mfminimizer
	  (minimizer *simplex-nelder-mead* parabaloid-f initial step-size 2)
	(loop with status = T and size
	      for iter from 0 below 100
	      while status
	      do (iterate-mfminimizer minimizer)
	      (setf size
		    (mfminimizer-size minimizer)
		    status
		    (not (min-test-size size 1.0d-2)))
	      (let ((x (mfminimizer-x minimizer)))
		(format t "~&~d~6t~10,6f~18t~10,6f~28t~12,9f~40t~8,3f"
			iter (gsl-aref x 0) (gsl-aref x 1)
			(mfminimizer-minimum minimizer)
			size))
	      finally (return
			(let ((x (mfminimizer-x minimizer)))
			  (values (gsl-aref x 0) (gsl-aref x 1)))))))))
