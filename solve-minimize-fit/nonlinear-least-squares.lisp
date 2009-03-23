;; Nonlinear least squares fitting.
;; Liam Healy, 2008-02-09 12:59:16EST nonlinear-least-squares.lisp
;; Time-stamp: <2009-03-22 16:09:53EDT nonlinear-least-squares.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_multifit_nlin.h

;;;;****************************************************************************
;;;; Function-only solver object
;;;;****************************************************************************

;;; Note that GSL currently provides no derivative-free solvers,
;;; so this is moot for now.

(defmobject nonlinear-ffit "gsl_multifit_fsolver"
  ((solver-type :pointer)
   ((first dimensions) sizet)		; number-of-observations
   ((second dimensions) sizet))		; number-of-parameters
  "nonlinear least squares fit with function only"
  :documentation			; FDL
  "The number of observations must be greater than or equal to parameters."
  :callbacks
  (callback gsl-ffit-function
	    (number-of-observations number-of-parameters)
	    (function :success-failure
		      (:double :marray dim1) (:double :marray dim0)))
  :initialize-suffix "set"
  :initialize-args ((callback :pointer) ((mpointer initial-guess) :pointer))
  :singular (function))

(cffi:defcstruct gsl-ffit-function
  "The definition of a function for nonlinear least squares fitting in GSL."
  ;; See gsl_multifit_function in /usr/include/gsl/gsl_multifit_nlin.h
  (function :pointer)
  (number-of-observations sizet)
  (number-of-parameters sizet)
  (parameters :pointer))

(def-make-callbacks nonlinear-ffit
    (function number-of-observations number-of-parameters &optional (scalars t))
  (if scalars
      `(defmcallback ,function
	   :success-failure
	 (:double ,number-of-parameters)
	 ((:double ,number-of-observations))
	 t ,function)
      `(defmcallback ,function
	   :success-failure :pointer :pointer nil ,function)))

(defmfun name ((solver nonlinear-ffit))
  "gsl_multifit_fsolver_name"
  (((mpointer solver) :pointer))
  :definition :method
  :c-return :string
  :documentation			; FDL
  "The name of the solver type.")

;;;;****************************************************************************
;;;; Function and derivative solver object
;;;;****************************************************************************

(defmobject nonlinear-fdffit "gsl_multifit_fdfsolver"
  ((solver-type :pointer)
   ((first dimensions) sizet)		; number-of-observations
   ((second dimensions) sizet))		; number-of-parameters
  "nonlinear least squares fit with function and derivative"
  :documentation			; FDL
  "The number of observations must be greater than or
   equal to parameters."
  :callbacks
  (callback gsl-fdffit-function
	    (number-of-observations number-of-parameters)
	    (function :success-failure
		      (:double :marray dim1) (:double :marray dim0))
	    (df :success-failure
		      (:double :marray dim1) (:double :marray dim0 dim1))
	    (fdf :success-failure
		      (:double :marray dim1)
		      (:double :marray dim0) (:double :marray dim0 dim1)))
  :initialize-suffix "set"
  :initialize-args ((callback :pointer) ((mpointer initial-guess) :pointer)))

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

(cffi:defcstruct gsl-fdffit-function
  "The definition of a function and its derivatives for nonlinear
   least squares fitting in GSL."
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (number-of-observations sizet)
  (number-of-parameters sizet)
  (parameters :pointer))

#|
(def-make-callbacks nonlinear-fdffit
    (function df fdf &optional number-of-observations number-of-parameters)
  (if number-of-observations
      (cl-utilities:once-only (number-of-parameters number-of-observations)
	`(progn
	   (defmcallback ,function
	       :success-failure
	     (:double ,number-of-parameters)
	     ((:double ,number-of-observations))
	     t ,function)
	   (defmcallback ,df
	       :success-failure
	     (:double ,number-of-parameters)
	     ((:double ,number-of-observations ,number-of-parameters))
	     t ,df)
	   (defmcallback ,fdf
	       :success-failure
	     (:double ,number-of-parameters)
	     ((:double ,number-of-observations)
	      (:double ,number-of-observations ,number-of-parameters))
	     t ,fdf)))
      `(progn
	 (defmcallback ,function
	     :success-failure :pointer :pointer nil ,function)
	 (defmcallback ,df :success-failure :pointer :pointer nil ,df)
	 (defmcallback
	     ,fdf :success-failure :pointer (:pointer :pointer) nil ,fdf))))
|#

(defmfun name ((solver nonlinear-fdffit))
  "gsl_multifit_fdfsolver_name"
  (((mpointer solver) :pointer))
  :definition :method
  :c-return :string
  :documentation			; FDL
  "The name of the solver type.")

;;;;****************************************************************************
;;;; Iteration
;;;;****************************************************************************

(defmfun iterate ((solver nonlinear-ffit))
  "gsl_multifit_fsolver_iterate"
  (((mpointer solver) :pointer))
  :definition :method
  :documentation			; FDL
  "Perform a single iteration of the solver.  The solver maintains a
   current estimate of the best-fit parameters at all times. ")

(defmfun iterate ((solver nonlinear-fdffit))
  "gsl_multifit_fdfsolver_iterate"
  (((mpointer solver) :pointer))
  :definition :method
  :documentation			; FDL
  "Perform a single iteration of the solver.  The solver maintains a
   current estimate of the best-fit parameters at all times. ")

(defmfun solution ((solver nonlinear-ffit))
  "gsl_multifit_fsolver_position"
  (((mpointer solver) :pointer))
  :definition :method
  :c-return (crtn :pointer)
  :return ((copy crtn))
  :documentation			; FDL
  "The current best-fit parameters.")

(defmfun solution ((solver nonlinear-fdffit))
  "gsl_multifit_fdfsolver_position"
  (((mpointer solver) :pointer))
  :definition :method
  :c-return (crtn :pointer)
  :return ((copy crtn))
  :documentation			; FDL
  "The current best-fit parameters.")

;;; Why doesn't GSL have functions to extract these values?
(defmethod function-value ((solver nonlinear-fdffit))
  (copy
   (cffi:foreign-slot-value (mpointer solver) 'gsl-fdffit-solver 'f)))

(defmethod last-step ((solver nonlinear-fdffit))
  ;; Raw pointer, because we presume we're passing it on to another GSL function. 
  (cffi:foreign-slot-value (mpointer solver) 'gsl-fdffit-solver 'dx))

(defun jacobian (solver)
  ;; Raw pointer, because we presume we're passing it on to another GSL function. 
  (cffi:foreign-slot-value (mpointer solver) 'gsl-fdffit-solver 'jacobian))

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
  ((jacobian :pointer) ((mpointer function-values) :pointer) (gradient :pointer))
  :documentation			; FDL
  "Compute the gradient of \Phi(x) = (1/2) ||F(x)||^2
   from the Jacobian matrix and the function values using
   the formula g = J^T f.")

;;;;****************************************************************************
;;;; Minimization using derivatives
;;;;****************************************************************************

(defmpar +levenberg-marquardt+ "gsl_multifit_fdfsolver_lmsder"
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

   The algorithm also monitors the progress of the solution and
   returns an error if the changes in the solution are smaller than
   the machine precision.  The possible errors signalled are:
   'failure-to-reach-tolerance-f the decrease in the function falls
   below machine precision,
   'failure-to-reach-tolerance-x
    the change in the position vector falls below machine precision,
   'failure-to-reach-tolerance-g
    the norm of the gradient, relative to the norm of the function,
   falls below machine precision.
   These errors indicate that further iterations would be unlikely to
   change the solution from its current value.")

(defmpar +levenberg-marquardt-unscaled+ "gsl_multifit_fdfsolver_lmder"
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
  ((jacobian :pointer) (relative-error :double) ((mpointer covariance) :pointer))
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

(defstruct exponent-fit-data n y sigma)
(defvar *nlls-example-data*)

(defun generate-nlls-data (&optional (number-of-observations 40))
  "Create the data used in the nonlinear least squares fit example."
  (make-exponent-fit-data
   :n number-of-observations
   :y
   (let ((arr (make-marray 'double-float :dimensions number-of-observations))
	 (rng (make-random-number-generator +mt19937+ 0)))
     (dotimes (i number-of-observations arr)
       (setf (maref arr i)
	     (+ 1 (* 5 (exp (* -1/10 i))) (gaussian rng 0.1d0)))))
   :sigma
   (make-marray
    'double-float :dimensions number-of-observations :initial-element 0.1d0)))

(defun exponential-residual (x f)
  "Compute the negative of the residuals with the exponential model
   for the nonlinear least squares example."
  (let ((A (maref x 0))
	(lambda (maref x 1))
	(b (maref x 2)))
    (symbol-macrolet
	((y (exponent-fit-data-y *nlls-example-data*))
	 (sigma (exponent-fit-data-sigma *nlls-example-data*)))
      (dotimes (i (exponent-fit-data-n *nlls-example-data*))
	(setf (maref f i)
	      ;; the difference model - observation = - residual
	      (/ (- (+ (* A (exp (* (- lambda) i))) b) (maref y i))
		 (maref sigma i)))))))

(defun exponential-residual-derivative (x jacobian)
  "Compute the partial derivatives of the negative of the
   residuals with the exponential model
   for the nonlinear least squares example."
  (let ((A (maref x 0))
	(lambda (maref x 1)))
    (symbol-macrolet
	  ((sigma (exponent-fit-data-sigma *nlls-example-data*)))
	(dotimes (i (exponent-fit-data-n *nlls-example-data*))
	  (let ((e (exp (* (- lambda) i)))
		(s (maref sigma i)))
	  (setf (maref jacobian i 0) (/ e s)
		(maref jacobian i 1) (* -1 i A (/ e s))
		(maref jacobian i 2) (/ s)))))))

(defun exponential-residual-fdf (x f jacobian)
  "Compute the function and partial derivatives of the negative of the
   residuals with the exponential model
   for the nonlinear least squares example."
  (exponential-residual x f)
  (exponential-residual-derivative x jacobian))

#|
(make-callbacks
 nonlinear-fdffit
 exponential-residual exponential-residual-derivative
 exponential-residual-fdf)
|#

(defun norm-f (fit)
  "Find the norm of the fit function f."
  (euclidean-norm (function-value fit)))

(defun nonlinear-least-squares-example
    (&optional (number-of-observations 40)
     (method +levenberg-marquardt+)
     (print-steps t))
  (let ((*nlls-example-data* (generate-nlls-data number-of-observations)))
    (let* ((init #m(1.0d0 0.0d0 0.0d0))
	   (number-of-parameters 3)
	   (covariance
	    (make-marray 'double-float
			 :dimensions
			 (list number-of-parameters number-of-parameters)))
	   (fit (make-nonlinear-fdffit
		 method
		 (list number-of-observations number-of-parameters)
		 '(exponential-residual
		   exponential-residual-derivative exponential-residual-fdf)
		 init nil)))
      (macrolet ((fitx (i) `(maref (solution fit) ,i))
		 (err (i) `(sqrt (maref covariance ,i ,i))))
	(when print-steps
	  (format t "iter: ~d x = ~15,8f ~15,8f ~15,8f |f(x)|=~7,6g~&"
		  0 (fitx 0) (fitx 1) (fitx 2)
		  (norm-f fit)))
	(loop for iter from 0 below 25
	   until
	   (and (plusp iter)
		(fit-test-delta (last-step fit) (mpointer (solution fit)) 1.0d-4 1.0d-4))
	   do
	   (iterate fit)
	   (ls-covariance (jacobian fit) 0.0d0 covariance)
	   (when print-steps
	     (format t "iter: ~d x = ~15,8f ~15,8f ~15,8f |f(x)|=~7,6g~&"
		     (1+ iter) (fitx 0) (fitx 1) (fitx 2)
		     (norm-f fit)))
	   finally
	   (let* ((chi (norm-f fit))
		  (dof (- number-of-observations number-of-parameters))
		  (c (max 1.0d0 (/ chi (sqrt dof)))))
	     (when print-steps
	       (format t "chisq/dof = ~g~&" (/ (expt chi 2) dof))
	       (format t "A         = ~,5f +/- ~,5f~&" (fitx 0) (* c (err 0)))
	       (format t "lambda    = ~,5f +/- ~,5f~&" (fitx 1) (* c (err 1)))
	       (format t "b         = ~,5f +/- ~,5f~&" (fitx 2) (* c (err 2))))
	     (return (list (fitx 0) (fitx 1) (fitx 2)))))))))

(save-test nonlinear-least-squares
	   (nonlinear-least-squares-example 40 +levenberg-marquardt+ nil))
