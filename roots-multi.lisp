;;; Multivariate roots.                
;;; Liam Healy 2008-01-12 12:49:08
;;; Time-stamp: <2008-01-13 22:51:02 liam roots-multi.lisp>
;;; $Id: $

(in-package :gsl)

;;; I don't like using make-data-from-pointer.

;;;;****************************************************************************
;;;; Function definition
;;;;****************************************************************************

(cffi:defcstruct gsl-mfunction
  ;; See /usr/include/gsl/gsl_multiroots.h
  "The definition of a function for multiroot finding in GSL."
  (function :pointer)
  (dimensions :size)
  (parameters :pointer))

(export 'def-mfunction)
(defmacro def-mfunction (name dimensions)
  "Define a function for multivariate root solving."
  `(def-scalar-function ,name :success-failure :pointer gsl-mfunction
    ((dimensions ,dimensions))
    ((returned-value gsl-vector-c))))

;;;;****************************************************************************
;;;; Initialization
;;;;****************************************************************************

(defun-gsl allocate-mfsolver (type dimension)
  "gsl_multiroot_fsolver_alloc"
  ((type :pointer) (dimension :size))
  :c-return :pointer
  :documentation
  "Allocate an instance of a solver of the type specified for a system of
   the specified number of dimensions.")

(defun-gsl allocate-mfdfsolver (type dimension)
  "gsl_multiroot_fdfsolver_alloc"
  ((type :pointer) (dimension :size))
  :c-return :pointer
  :documentation
  "Allocate an instance of a derivative solver of the type specified for
   a system of the specified number of dimensions.")

(defun-gsl set-mfsolver (solver function initial)
  "gsl_multiroot_fsolver_set"
  ((solver :pointer) (function :pointer) ((pointer initial) :pointer))
  :documentation
  "Set or reset an existing solver to use the function and the
   initial guess gsl-vector.")

(defun-gsl set-mfdfsolver (solver function-derivative initial)
  "gsl_multiroot_fdfsolver_set"
  ((solver :pointer) (function-derivative :pointer)
   ((gsl-array initial) :pointer))
  :documentation
  "Set or reset an existing solver to use the function and derivative
   (fdf) and the initial guess.")

(defun-gsl free-mfsolver (solver)
  "gsl_multiroot_fsolver_free"
  ((solver :pointer))
  :c-return :void
  :documentation
  "Free all the memory associated with the solver.")

(defun-gsl free-mfdfsolver (solver)
  "gsl_multiroot_fdfsolver_free"
  ((solver :pointer))
  :c-return :void
  :documentation
  "Free all the memory associated with the solver.")

(defun-gsl mfsolver-name (solver)
  "gsl_multiroot_fsolver_name"
  ((solver :pointer))
  :c-return :string
  :documentation
  "The name of the solver.")

(defun-gsl mfdfsolver-name (solver)
  "gsl_multiroot_fdfsolver_name"
  ((solver :pointer))
  :c-return :string
  :documentation
  "The name of the solver.")

(export '(with-mfsolver with-mfdfsolver))
(defmacro with-mfsolver ((solver solver-type function initial) &body body)
  "Create and initialize an fsolver for multi-dimensional problems,
   and clean up afterwards."
  `(let ((,solver (allocate-mfsolver ,solver-type (dim0 ,initial))))
    (unwind-protect
	 (progn
	   (set-mfsolver ,solver ,function ,initial)
	   ,@body)
      (free-mfsolver ,solver))))

(defmacro with-mfdfsolver
    ((solver solver-type f-df-fdf root-guess) &body body)
  "Create and initialize an fdfsolver for one-dimensional problems,
   and clean up afterwards."
  `(let ((,solver (allocate-mfdfsolver ,solver-type (dim0 ,root-guess))))
    (unwind-protect
	 (progn
	   (set-mfdfsolver ,solver ,f-df-fdf ,root-guess)
	   ,@body)
      (free-mfdfsolver ,solver))))

;;;;****************************************************************************
;;;; Iteration
;;;;****************************************************************************

(defun-gsl iterate-mfsolver (solver)
  "gsl_multiroot_fsolver_iterate"
  ((solver :pointer))
  :documentation
  "Perform a single iteration of the solver.  The following
   errors may be signalled: :EBADFUNC,
   the iteration encountered a singular point where the function or its
   derivative evaluated to infinity or NaN, or
   :EZERODIV, the derivative of the function vanished at the iteration point,
   preventing the algorithm from continuing without a division by zero.")

(defun-gsl iterate-mfdfsolver (solver)
  "gsl_multiroot_fdfsolver_iterate"
  ((solver :pointer))
  :documentation
  "Perform a single iteration of the solver.  The following
   errors may be signalled: :EBADFUNC,
   the iteration encountered a singular point where the function or its
   derivative evaluated to infinity or NaN, or
   :EZERODIV, the derivative of the function vanished at the iteration point,
   preventing the algorithm from continuing without a division by zero.")

(defun-gsl mfsolver-root (solver)
  "gsl_multiroot_fsolver_root"
  ((solver :pointer))
  :c-return (canswer :pointer)
  :return ((make-data-from-pointer canswer))
  :documentation
  "The current estimate of the root for the solver.")

(defun-gsl mfdfsolver-root (solver)
  "gsl_multiroot_fdfsolver_root"
  ((solver :pointer))
  :c-return (canswer gsl-vector-c)
  :return ((make-data-from-pointer canswer))
  :documentation
  "The current estimate of the root for the solver.")

(defun-gsl mfsolver-f (solver)
  "gsl_multiroot_fsolver_f"
  ((solver :pointer))
  :c-return (canswer gsl-vector-c)
  :return ((make-data-from-pointer canswer))
  :documentation
  "The function value f(x) at the current estimate x of the root for the solver.")

(defun-gsl mfdfsolver-f (solver)
  "gsl_multiroot_fdfsolver_f"
  ((solver :pointer))
  :c-return (canswer gsl-vector-c)
  :return ((make-data-from-pointer canswer))
  :documentation
  "The function value f(x) at the current estimate x of the root for the solver.")

(defun-gsl mfsolver-dx (solver)
  "gsl_multiroot_fsolver_dx"
  ((solver :pointer))
  :c-return (canswer gsl-vector-c)
  :return ((make-data-from-pointer canswer))
  :documentation
  "The last step dx taken by the solver.")

(defun-gsl mfdfsolver-dx (solver)
  "gsl_multiroot_fsolver_dx"
  ((solver :pointer))
  :c-return (canswer gsl-vector-c)
  :return ((make-data-from-pointer canswer))
  :documentation
  "The last step dx taken by the solver.")

;;;;****************************************************************************
;;;; Search stopping conditions
;;;;****************************************************************************

;;; The only place we need to pick apart the gsl_multiroot_fsolver
;;; struct is here.  We could use mfsolver-dx etc., but then we'd have
;;; to discriminate on mfsolver vs. mfdfsolver.
(cffi:defcstruct gsl-multiroot-fsolver
  ;; See /usr/include/gsl/gsl_multiroots.h
  (type :pointer)
  (function :pointer)
  (x :pointer)
  (f :pointer)
  (dx :pointer)
  (state :pointer))

(defun multiroot-slot (solver slot)
  (cffi:foreign-slot-value solver 'gsl-multiroot-fsolver slot))

(defun-gsl multiroot-test-delta (solver absolute-error relative-error)
  "gsl_multiroot_test_delta"
  (((multiroot-slot solver 'dx) :pointer)
   ((multiroot-slot solver 'x) :pointer)
   (absolute-error :double) (relative-error :double))
  :c-return :success-continue
  :documentation
  "Test for the convergence of the sequence by comparing the
   last step dx with the absolute error and relative
   errors given to the current position x.  The test returns
   T if the following condition is achieved:
   |dx_i| < epsabs + epsrel |x_i|
   for each component of x and returns NIL otherwise.")

(defun-gsl multiroot-test-residual (solver absolute-error)
  "gsl_multiroot_test_residual"
  (((multiroot-slot solver 'f) :pointer) (absolute-error :double))
  :c-return :success-failure
  :documentation
  "Test the residual value f against the absolute error,
   returning T if the following condition is achieved:
   \sum_i |f_i| < absolute_error
   and returns NIL otherwise.  This criterion is suitable
   for situations where the precise location of the root x is
   unimportant provided a value can be found where the
   residual is small enough.")

;;;;****************************************************************************
;;;; Algorithms using derivatives
;;;;****************************************************************************

(defvariable *powells-hybrid* "gsl_multiroot_fdfsolver_hybridsj"
  "This is a modified version of Powell's Hybrid method as implemented in
   the hybrj algorithm in @sc{minpack}.  Minpack was written by Jorge
   J. More, Burton S. Garbow and Kenneth E. Hillstrom.  The Hybrid
   algorithm retains the fast convergence of Newton's method but will also
   reduce the residual when Newton's method is unreliable. 

   The algorithm uses a generalized trust region to keep each step under
   control.  In order to be accepted a proposed new position x' must
   satisfy the condition |D (x' - x)| < \delta, where D is a
   diagonal scaling matrix and \delta is the size of the trust
   region.  The components of D are computed internally, using the
   column norms of the Jacobian to estimate the sensitivity of the residual
   to each component of x.  This improves the behavior of the
   algorithm for badly scaled functions.

   On each iteration the algorithm first determines the standard Newton
   step by solving the system J dx = - f.  If this step falls inside
   the trust region it is used as a trial step in the next stage.  If not,
   the algorithm uses the linear combination of the Newton and gradient
   directions which is predicted to minimize the norm of the function while
   staying inside the trust region,
   dx = - \alpha J^{-1} f(x) - \beta \nabla |f(x)|^2.
   This combination of Newton and gradient directions is referred to as a
   dogleg step.

   The proposed step is now tested by evaluating the function at the
   resulting point, x'.  If the step reduces the norm of the function
   sufficiently then it is accepted and size of the trust region is
   increased.  If the proposed step fails to improve the solution then the
   size of the trust region is decreased and another trial step is
   computed.

   The speed of the algorithm is increased by computing the changes to the
   Jacobian approximately, using a rank-1 update.  If two successive
   attempts fail to reduce the residual then the full Jacobian is
   recomputed.  The algorithm also monitors the progress of the solution
   and returns an error if several steps fail to make any improvement,
   :ENOPROG
   the iteration is not making any progress, preventing the algorithm from
   continuing.
   :ENOPROGJ
   re-evaluations of the Jacobian indicate that the iteration is not
   making any progress, preventing the algorithm from continuing.")

(defvariable *powells-hybrid-unscaled* "gsl_multiroot_fdfsolver_hybridj"
  "This algorithm is an unscaled version of *powells-hybrid*.  The steps are
   controlled by a spherical trust region |x' - x| < \delta, instead
   of a generalized region.  This can be useful if the generalized region
   estimated by *powells-hybrid* is inappropriate.")

(defvariable *newton-mfdfsolver* "gsl_multiroot_fdfsolver_newton"
  "Newton's Method is the standard root-polishing algorithm.  The algorithm
   begins with an initial guess for the location of the solution.  On each
   iteration a linear approximation to the function F is used to
   estimate the step which will zero all the components of the residual.
   The iteration is defined by the following sequence,
   x -> x' = x - J{-1} f(x)
   where the Jacobian matrix J is computed from the derivative
   functions provided by f.  The step dx is obtained by solving
   the linear system,
   J dx = - f(x)
   using LU decomposition.")

(defvariable *gnewton-mfdfsolver* "gsl_multiroot_fdfsolver_gnewton"
  "A modified version of Newton's method which attempts to improve
   global convergence by requiring every step to reduce the Euclidean norm
   of the residual, |f(x)|.  If the Newton step leads to an increase
   in the norm then a reduced step of relative size,
   t = (\sqrt(1 + 6 r) - 1) / (3 r)
   is proposed, with r being the ratio of norms
   |f(x')|^2/|f(x)|^2.  This procedure is repeated until a suitable step
   size is found.")

;;;;****************************************************************************
;;;; Algorithms without derivatives
;;;;****************************************************************************

(defvariable *hybrid-scaled* "gsl_multiroot_fsolver_hybrids"
    "This is a version of the Hybrid algorithm which replaces calls to the
     Jacobian function by its finite difference approximation.  The finite
     difference approximation is computed using gsl_multiroots_fdjac
     with a relative step size of GSL_SQRT_DBL_EPSILON.")
;; Where is this function and parameter?  Only thing that shows in the
;; library is gsl_multiroot_fdjacobian.
 
(defvariable *hybrid-unscaled* "gsl_multiroot_fsolver_hybrid"
  "A finite difference version of the Hybrid algorithm without
   internal scaling.")

(defvariable *discrete-newton* "gsl_multiroot_fsolver_dnewton"
  "The discrete Newton algorithm is the simplest method of solving a
   multidimensional system.  It uses the Newton iteration
   x -> x - J^{-1} f(x)
   where the Jacobian matrix J is approximated by taking finite
   differences of the function f.  The approximation scheme used by
   this implementation is
   J_{ij} = (f_i(x + \delta_j) - f_i(x)) /  \delta_j
   where \delta_j is a step of size \sqrt\epsilon |x_j| with
   \epsilon being the machine precision 
   (\epsilon \approx 2.22 \times 10^-16}).
   The order of convergence of Newton's algorithm is quadratic, but the
   finite differences require n^2 function evaluations on each
   iteration.  The algorithm may become unstable if the finite differences
   are not a good approximation to the true derivatives.")

(defvariable *broyden* "gsl_multiroot_fsolver_broyden"
  "The Broyden algorithm is a version of the discrete Newton
   algorithm which attempts to avoids the expensive update of the Jacobian
   matrix on each iteration.  The changes to the Jacobian are also
   approximated, using a rank-1 update,
   J^{-1} \to J^{-1} - (J^{-1} df - dx) dx^T J^{-1} / dx^T J^{-1} df
   where the vectors dx and df are the changes in x
   and f.  On the first iteration the inverse Jacobian is estimated
   using finite differences, as in the discrete Newton algorithm.
    
   This approximation gives a fast update but is unreliable if the changes
   are not small, and the estimate of the inverse Jacobian becomes worse as
   time passes.  The algorithm has a tendency to become unstable unless it
   starts close to the root.  The Jacobian is refreshed if this instability
   is detected (consult the source for details).

   This algorithm is included only for demonstration purposes, and is not
   recommended for serious use.")

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

(defparameter *powell-A* 1.0d4)
(defun powell (argument return)
  "Powell's test function."
  (setf (gsl-aref return 0)
	(- (* *powell-A* (gsl-aref argument 0) (gsl-aref argument 1))
	   1)
	(gsl-aref return 1)
	(+ (exp (- (gsl-aref argument 0))) (exp (- (gsl-aref argument 1)))
	   (- (1+ (/ *powell-A*))))))

;;; (def-mfunction powell 2)

;;; This is the example given in Sec. 34.8.

(defparameter *rosenbrock-a* 1.0d0)
(defparameter *rosenbrock-b* 10.0d0)

(defparameter *gsl-vector*
  (make-instance 'gsl-vector-double :pointer nil :storage-size nil))

(defun rosenbrock (argument return)
  "Rosenbrock test function."
  (with-c-doubles (((gsl-array-p argument) x0 x1)
		   ((gsl-array-p return) f0 f1))
    (setf f0 (* *rosenbrock-a* (- 1 x0))
	  f1 (* *rosenbrock-b* (- x1 (expt x0 2))))))

(def-mfunction rosenbrock 2)

(defun roots-multi-example ()
  "Solving Rosenbrock, the example given in Sec. 34.8 of the GSL manual."
  (let ((max-iter 1000))
    (with-data (vect vector-double 2)
      (setf (data vect) #(-10.0d0 -5.0d0))
      (with-mfsolver (solver *hybrid-scaled* rosenbrock vect)
	(let ((fnval (mfsolver-f solver))
	      (argval (mfsolver-root solver)))
	  (loop for iter from 0
		while (and (< iter max-iter)
			   (not (multiroot-test-residual solver 1.0d-7)))
		do
		(iterate-mfsolver solver)
		(format t "~&iter=~d~8tx0=~12,8g~24tx1=~12,8g~38tf0=~12,8g~52tf1=~12,8g"
			iter
			(gsl-aref argval 0)
			(gsl-aref argval 1)
			(gsl-aref fnval 0)
			(gsl-aref fnval 1))
		finally (return
			  (values (gsl-aref argval 0)
				  (gsl-aref argval 1)
				  (gsl-aref fnval 0)
				  (gsl-aref fnval 1)))))))))
