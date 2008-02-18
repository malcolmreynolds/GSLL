;; Nonlinear least squares fitting.
;; Liam Healy, 2008-02-09 12:59:16EST nonlinear-least-squares.lisp
;; Time-stamp: <2008-02-17 18:26:44EST nonlinear-least-squares.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Function-only solver object
;;;;****************************************************************************

;;; Note that there are currently no derivative-free solvers provided,
;;; so this is a bit pointless.

(defgo-s (nonlinear-ffit solver-type number-of-observations number-of-parameters
			 function initial-guess)
    allocate-ffit free-ffit set-ffit 3)

(defmfun allocate-ffit (solver-type number-of-observations number-of-parameters)
  "gsl_multifit_fsolver_alloc"
  ((solver-type :pointer) (number-of-observations size) (number-of-parameters size))
  :c-return :pointer
  :export nil
  :index (letm nonlinear-ffit)
  :documentation			; FDL
  "Allocate an instance of a solver.  The number of observations
   must be greater than or equal to parameters.")

(defmfun set-ffit (solver function initial-guess)
  "gsl_multifit_fsolver_set"
  ((solver :pointer) (function :pointer) (initial-guess :pointer))
  :documentation			; FDL
  "Initialize or reinitialize an existing solver
   to use the function and the initial guess.")

(defmfun free-ffit (solver)
  "gsl_multifit_fsolver_free"
  ((solver :pointer))
  :c-return :void
  :export nil
  :index (letm nonlinear-ffit)
  :documentation			; FDL
  "Free all the memory associated with the solver.")

(defmfun name-ffit (solver)
  "gsl_multifit_fsolver_name"
  ((solver :pointer))
  :c-return :string
  :documentation			; FDL
  "The name of the solver type.")

;;;;****************************************************************************
;;;; Function and derivative solver object
;;;;****************************************************************************

(defgo-s (nonlinear-fdffit solver-type number-of-observations number-of-parameters
			   functions initial-guess)
    allocate-fdffit free-fdffit set-fdffit 3)

(defmfun allocate-fdffit (solver-type number-of-observations number-of-parameters)
  "gsl_multifit_fdfsolver_alloc"
  ((solver-type :pointer) (number-of-observations size) (number-of-parameters size))
  :c-return :pointer
  :export nil
  :index (letm nonlinear-fdffit)
  :documentation			; FDL
  "Allocate an instance of a solver.  The number of observations
   must be greater than or equal to parameters.")

(defmfun set-fdffit (solver function initial-guess)
  "gsl_multifit_fdfsolver_set"
  ((solver :pointer) (function :pointer) ((pointer initial-guess) :pointer))
  :documentation			; FDL
  "Initialize or reinitialize an existing solver
   to use the function and the initial guess.")

(defmfun free-fdffit (solver)
  "gsl_multifit_fdfsolver_free"
  ((solver :pointer))
  :c-return :void
  :export nil
  :index (letm nonlinear-fdffit)
  :documentation			; FDL
  "Free all the memory associated with the solver.")

(defmfun name-fdffit (solver)
  "gsl_multifit_fdfsolver_name"
  ((solver :pointer))
  :c-return :string
  :documentation			; FDL
  "The name of the solver type.")

(cffi:defcstruct gsl-fdffit-solver
  ;; See /usr/include/gsl/gsl_multifit_nlin.h
  "The definition of a solver instance and state
   for nonlinear least squares fitting in GSL."
  (type :pointer)
  (fdf :pointer)
  (x :pointer)
  (f :pointer)
  (jacobian :pointer)
  (dx :pointer)
  (state :pointer))

(export '(fdffit-slot))
(defun fdffit-slot (solver slot)
  (cffi:foreign-slot-value solver 'gsl-fdffit-solver slot))

;;;;****************************************************************************
;;;; The function to be minimized
;;;;****************************************************************************

(cffi:defcstruct gsl-ffit-function
  ;; See /usr/include/gsl/gsl_multifit_nlin.h
  "The definition of a function for nonlinear least squares fitting in GSL."
  (function :pointer)
  (dimensions size)
  (parameters :pointer))

(cffi:defcstruct gsl-fdffit-function
  ;; See 
  "The definition of a function and its derivatives for nonlinear
   least squares fitting in GSL."
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (number-of-observations size)
  (number-of-parameters size)
  (parameters :pointer))

(export 'def-fitting-functions)
(defmacro def-fitting-functions
    (function number-of-observations number-of-parameters &optional df fdf)
  "Setup functions for nonlinear least squares fitting.
   The CL functions name and derivative should be defined previously
   with defuns."
  `(progn
    (defmcallback ,function :success-failure :pointer :pointer)
    ,@(when df
	    `((defmcallback ,df :success-failure :pointer :pointer)
	      (defmcallback ,fdf :success-failure :pointer (:pointer :pointer))))
    ,(if df
	 `(defcbstruct (,function function ,df df ,fdf fdf)
	   gsl-fdffit-function
	   ((number-of-observations ,number-of-observations)
	    (number-of-parameters ,number-of-parameters)))
	 `(defcbstruct (,function function)
	   gsl-ffit-function
	   ((number-of-observations ,number-of-observations)
	    (number-of-parameters ,number-of-parameters))))))

;;;;****************************************************************************
;;;; Iteration
;;;;****************************************************************************

(defmfun iterate-ffit (solver)
  "gsl_multifit_fsolver_iterate"
  ((solver :pointer))
  :documentation			; FDL
  "Perform a single iteration of the solver.  The solver maintains a
   current estimate of the best-fit parameters at all times. ")

(defmfun iterate-fdffit (solver)
  "gsl_multifit_fdfsolver_iterate"
  ((solver :pointer))
  :documentation			; FDL
  "Perform a single iteration of the solver.  The solver maintains a
   current estimate of the best-fit parameters at all times. ")

(defmfun current-parameters-ffit (solver)
  "gsl_multifit_fsolver_position"
  ((solver :pointer))
  :c-return (canswer :pointer)
  :return ((make-data-from-pointer canswer))
  :documentation			; FDL
  "The current best-fit parameters.")

(defmfun current-parameters-fdffit (solver)
  "gsl_multifit_fdfsolver_position"
  ((solver :pointer))
  :c-return (canswer :pointer)
  :return ((make-data-from-pointer canswer))
  :documentation			; FDL
  "The current best-fit parameters.")

;;;;****************************************************************************
;;;; Search stopping
;;;;****************************************************************************

(defmfun fit-test-delta
    (last-step current-position absolute-error relative-error)
  "gsl_multifit_test_delta"
  ((last-step :pointer) (current-position :pointer)
   (absolute-error :double) (relative-error :double))
  :c-return :success-continue
  :documentation			; FDL
  "Test for the convergence of the sequence by comparing the
   last step with the absolute error and relative
   error to the current position.  The test returns T
   if |last-step_i| < absolute-error + relative-error |current-position_i|
   for each component i of current-position and returns NIL otherwise.")

(defmfun fit-test-gradient (gradient absolute-error)
  "gsl_multifit_test_gradient"
  ((gradient :pointer) (absolute-error :double))
  :c-return :success-continue
  :documentation			; FDL
  "Test the residual gradient against the absolute
   error bound.  Mathematically, the gradient should be
   exactly zero at the minimum. The test returns T if the
   following condition is achieved: \sum_i |gradient_i| < absolute-error
   and returns NIL otherwise.  This criterion is suitable
   for situations where the precise location of the minimum
   is unimportant provided a value can be found where the gradient is small
   enough.")

(defmfun fit-gradient (jacobian function-values gradient)
  "gsl_multifit_gradient"
  ((jacobian :pointer) ((pointer function-values) :pointer) (gradient :pointer))
  :documentation			; FDL
  "Compute the gradient of \Phi(x) = (1/2) ||F(x)||^2
   from the Jacobian matrix and the function values using
   the formula g = J^T f.")

;;;;****************************************************************************
;;;; Minimization using derivatives
;;;;****************************************************************************

(defmpar *levenberg-marquardt* "gsl_multifit_fdfsolver_lmsder"
  ;; FDL
  "A robust and efficient version of the Levenberg-Marquardt
   algorithm as implemented in the scaled lmder routine in
   Minpack, written by Jorge J. More', Burton S. Garbow
   and Kenneth E. Hillstrom.

   The algorithm uses a generalized trust region to keep each step under
   control.  In order to be accepted a proposed new position x' must
   satisfy the condition |D (x' - x)| < \delta, where D is a
   diagonal scaling matrix and \delta is the size of the trust
   region.  The components of D are computed internally, using the
   column norms of the Jacobian to estimate the sensitivity of the residual
   to each component of x.  This improves the behavior of the
   algorithm for badly scaled functions.

   On each iteration the algorithm attempts to minimize the linear system
   |F + J p| subject to the constraint |D p| < \Delta.  The
   solution to this constrained linear system is found using the
   Levenberg-Marquardt method.

   The proposed step is now tested by evaluating the function at the
   resulting point, x'.  If the step reduces the norm of the
   function sufficiently, and follows the predicted behavior of the
   function within the trust region, then it is accepted and the size of the
   trust region is increased.  If the proposed step fails to improve the
   solution, or differs significantly from the expected behavior within
   the trust region, then the size of the trust region is decreased and
   another trial step is computed.

   The algorithm also monitors the progress of the solution and returns an
   error if the changes in the solution are smaller than the machine
   precision.  The possible error codes are,
   :ETOLF the decrease in the function falls below machine precision,
   :ETOLX the change in the position vector falls below machine precision,
   :ETOLG the norm of the gradient, relative to the norm of the function,
   falls below machine precision.
   These error codes indicate that further iterations will be unlikely to
   change the solution from its current value.")

(defmpar *levenberg-marquardt-unscaled* "gsl_multifit_fdfsolver_lmder"
  ;; FDL
  "The unscaled version of *levenberg-marquardt*.  The elements of the
   diagonal scaling matrix D are set to 1.  This algorithm may be
   useful in circumstances where the scaled version of converges too
   slowly, or the function is already scaled appropriately.")

;;;;****************************************************************************
;;;; Covariance
;;;;****************************************************************************

(defmfun ls-covariance (jacobian relative-error covariance)
  "gsl_multifit_covar"
  ((jacobian :pointer) (relative-error :double) ((pointer covariance) :pointer))
  :return (covariance)
  :documentation 			; FDL
  "Compute the covariance matrix of the best-fit parameters
   using the Jacobian matrix J.  The relative error
   is used to remove linear-dependent columns when J is
   rank deficient.  The covariance matrix is given by
   C = (J^T J)^{-1}
   and is computed by QR decomposition of J with column-pivoting.  Any
   columns of R which satisfy |R_{kk}| <= relative-error |R_{11}|
   are considered linearly-dependent and are excluded from the covariance
   matrix (the corresponding rows and columns of the covariance matrix are
   set to zero).

   If the minimisation uses the weighted least-squares function
   f_i = (Y(x, t_i) - y_i) / sigma_i then the covariance
   matrix above gives the statistical error on the best-fit parameters
   resulting from the gaussian errors sigma_i on 
   the underlying data y_i.  This can be verified from the relation 
   \delta f = J \delta c and the fact that the fluctuations in f
   from the data y_i are normalised by sigma_i and 
   so satisfy <delta f delta f^T> = I.

   For an unweighted least-squares function f_i = (Y(x, t_i) -
   y_i) the covariance matrix above should be multiplied by the variance
   of the residuals about the best-fit sigma^2 = sum (y_i - Y(x,t_i))^2 / (n-p)
   to give the variance-covariance matrix sigma^2 C.
   This estimates the statistical error on the
   best-fit parameters from the scatter of the underlying data.

   For more information about covariance matrices see the GSL documentation
   Fitting Overview.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************

;;; The example from Section 37.9 of the GSL manual.
;;; See the GSL source tree, doc/examples/expfit.c for the functions
;;; and doc/examples/nlfit.c for the solver.


(defparameter *number-of-observations* 40)
(defparameter *number-of-parameters* 3)
(defstruct exponent-fit-data n y sigma)
(defvar *nlls-example-data*)

(defun nlls-setup ()
  "Create the data used in the nonlinear least squares fit example."
  (setf
   *nlls-example-data*
   (make-exponent-fit-data
    :n *number-of-observations*
    :y
    (let ((arr (make-array *number-of-observations* :element-type 'double-float)))
      (letm ((rng (random-number-generator *mt19937* 0)))
	(dotimes (i *number-of-observations* arr)
	  (setf (aref arr i)
		(+ 1 (* 5 (exp (* -1/10 i))) (gaussian rng 0.1d0))))))
    :sigma
    (make-array *number-of-observations* :element-type 'double-float :initial-element 0.1d0))))

(defun exponential-residual (x f)
  "Compute the negative of the residuals with the exponential model
   for the nonlinear least squares example."
  (let ((A (vref x 0))
	(lambda (vref x 1))
	(b (vref x 2)))
    (symbol-macrolet
	  ((y (exponent-fit-data-y *nlls-example-data*))
	   (sigma (exponent-fit-data-sigma *nlls-example-data*)))
	(dotimes (i *number-of-observations*)
	  (setf (vref f i)
		;; the difference model - observation = - residual
		(/ (- (+ (* A (exp (* (- lambda) i))) b) (aref y i))
		   (aref sigma i)))))))

(defun exponential-residual-derivative (x jacobian)
  "Compute the partial derivatives of the negative of the
   residuals with the exponential model
   for the nonlinear least squares example."
  (let ((A (vref x 0))
	(lambda (vref x 1)))
    (symbol-macrolet
	  ((sigma (exponent-fit-data-sigma *nlls-example-data*)))
	(dotimes (i *number-of-observations*)
	  (let ((e (exp (* (- lambda) i)))
		(s (aref sigma i)))
	  (setf (mref jacobian i 0) (/ e s)
		(mref jacobian i 1) (* -1 i A (/ e s))
		(mref jacobian i 2) (/ s)))))))

(defun exponential-residual-fdf (x f jacobian)
  "Compute the function and partial derivatives of the negative of the
   residuals with the exponential model
   for the nonlinear least squares example."
  (exponential-residual x f)
  (exponential-residual-derivative x jacobian))

(def-fitting-functions
    exponential-residual *number-of-observations* *number-of-parameters*
    exponential-residual-derivative exponential-residual-fdf)

(defun solve-nonlinear-least-squares-example ()
  (letm ((init (vector-double #(1.0d0 0.0d0 0.0d0)))
	 (covariance
	  (matrix-double *number-of-parameters* *number-of-parameters*))
	 (fit (nonlinear-fdffit
	       *levenberg-marquardt*
	       *number-of-observations*
	       *number-of-parameters*
	       exponential-residual
	       init)))
    (macrolet ((fitx (i) `(vref (fdffit-slot fit 'x) ,i))
	       (err (i) `(sqrt (gsl-aref covariance ,i ,i))))
      (format t "~&iter: ~d x = ~15,8f ~15,8f ~15,8f |f(x)|=~7,6g"
	      0 (fitx 0) (fitx 1) (fitx 2)
	      (norm (make-data-from-pointer (fdffit-slot fit 'f))))
      (loop for iter from 0 below 25
	    until
	    (and (plusp iter)
		 (fit-test-delta
		  (fdffit-slot fit 'dx) (fdffit-slot fit 'x)
		  1.0d-4 1.0d-4))
	    do
	    (iterate-fdffit fit)
	    (ls-covariance (fdffit-slot fit 'jacobian) 0.0d0 covariance)
	    (format t "~&iter: ~d x = ~15,8f ~15,8f ~15,8f |f(x)|=~7,6g"
		    (1+ iter) (fitx 0) (fitx 1) (fitx 2)
		    (norm (make-data-from-pointer (fdffit-slot fit 'f))))
	    finally
	    (let* ((chi (norm (make-data-from-pointer (fdffit-slot fit 'f))))
		   (dof (- *number-of-observations* *number-of-parameters*))
		   (c (max 1.0d0 (/ chi (sqrt dof)))))
	      (format t "~&chisq/dof = ~g" (/ (expt chi 2) dof))
	      (format t "~&A         = ~,5f +/- ~,5f" (fitx 0) (* c (err 0)))
	      (format t "~&lambda    = ~,5f +/- ~,5f" (fitx 1) (* c (err 1)))
	      (format t "~&b         = ~,5f +/- ~,5f" (fitx 2) (* c (err 2)))
	      (return (list (fitx 0) (fitx 1) (fitx 2))))))))

;;; Run example:
;;; (nlls-setup)
;;; (solve-nonlinear-least-squares-example)
