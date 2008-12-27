;; Linear least squares, or linear regression
;; Liam Healy <2008-01-21 12:41:46EST linear-least-squares.lisp>
;; Time-stamp: <2008-12-26 18:38:44EST linear-least-squares.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_fit.h

;;;;****************************************************************************
;;;; Linear regression
;;;;****************************************************************************

;;; Error in GSL documentation or in code: vectors must be equal
;;; lengths but strides re not?  Shouldn't (floor length stride) be n?
;;; Error in GSL documentation for gsl_fit_linear_est, "c00" instead
;;; of "cov00" etc.  Last arg to gsl_fit_wmul is labelled sumsq but
;;; referred to as chisq.

(defmfun linear-fit (x y &optional (x-stride 1) (y-stride 1))
  "gsl_fit_linear"
  (((c-pointer x) :pointer) (x-stride sizet)
   ((c-pointer y) :pointer) (y-stride sizet)
   ((dim0 x) sizet) (c0 :double) (c1 :double)
   (cov00 :double) (cov01 :double) (cov11 :double)
   (sumsq :double))
  :inputs (x y)
  :documentation			; FDL
  "Determine the best-fit linear regression coefficients
   and returns as the first two values c0,c1
   of the model Y = c_0 + c_1 X for the dataset
   (x,y), two vectors of equal length with strides
   x-stride and y-stride.  The errors on y are assumed unknown so 
   the variance-covariance matrix for the
   parameters (c0, c1) is estimated from the scatter of the
   points around the best-fit line and returned as the third, fourth
   and fifth values cov00, cov01, cov11.   
   The sum of squares of the residuals from the best-fit line is returned
   as the final value.")

(defmfun weighted-linear-fit
    (x weight y &optional (x-stride 1) (weight-stride 1) (y-stride 1))
  "gsl_fit_wlinear"
  (((c-pointer x) :pointer) (x-stride sizet)
   ((c-pointer weight) :pointer) (weight-stride sizet)
   ((c-pointer y) :pointer) (y-stride sizet)
   ((dim0 x) sizet) (c0 :double) (c1 :double)
   (cov00 :double) (cov01 :double) (cov11 :double)
   (chisq :double))
  :inputs (x weight y)
  :documentation			; FDL
  "Compute the best-fit linear regression coefficients
   c0, c1 of the model Y = c_0 + c_1 X for the weighted
   dataset (x, y), two vectors of equal length with strides
   x-stride and y-stride, and return as the first two values.
   The vector weight, of the same length
   and stride w-stride, specifies the weight of each datapoint. The
   weight is the reciprocal of the variance for each datapoint in y.

   The covariance matrix for the parameters (c0, c1) is
   computed using the weights and returned via the parameters
   (cov00, cov01, c0v01) as the next three values.  The weighted sum
   of squares of the residuals from the best-fit line, \chi^2, is
   returned in as the last value.")

(defmfun linear-estimate (x c0 c1 cov00 cov01 cov11)
  "gsl_fit_linear_est"
  ((x :double) (c0 :double) (c1 :double)
   (cov00 :double) (cov01 :double) (cov11 :double)
   (y :double) (y-error :double))
  :documentation			; FDL
  "Use the best-fit linear regression coefficients
   c0, c1 and their covariance
   cov00, cov01, cov11 to compute the fitted function
   y and its standard deviation y-error for the model
   Y = c_0 + c_1 X at the point x.")

;;;;****************************************************************************
;;;; Linear fitting without a constant term
;;;;****************************************************************************

(defmfun multiplier-fit (x y &optional (x-stride 1) (y-stride 1))
  "gsl_fit_mul"
  (((c-pointer x) :pointer) (x-stride sizet)
   ((c-pointer y) :pointer) (y-stride sizet)
   ((dim0 x) sizet) (c1 :double) (cov11 :double)
   (sumsq :double))
  :inputs (x y)
  :documentation			; FDL
  "The best-fit linear regression coefficient c1 of the model Y = c_1
   X for the datasets (x, y) two vectors of equal length with strides x-stride
   and y-stride.  The errors on y are assumed unknown so the variance of
   the parameter c1 is estimated from the scatter of the points around
   the best-fit line and returned as the the second value.  The sum of
   squares of the residuals from the best-fit line is returned as the
   last value.")

(defmfun weighted-multiplier-fit
    (x weight y &optional (x-stride 1) (weight-stride 1) (y-stride 1))
  "gsl_fit_wmul"
  (((c-pointer x) :pointer) (x-stride sizet)
   ((c-pointer weight) :pointer) (weight-stride sizet)
   ((c-pointer y) :pointer) (y-stride sizet)
   ((dim0 x) sizet) (c1 :double) (cov11 :double)
   (chisq :double))
  :inputs (x weight y)
  :documentation			; FDL
  "Compute the best-fit linear regression coefficient
   c1 of the model Y = c_1 X for the weighted datasets
   (x, y), two vectors of equal length with strides
   x-stride and y-stride.  The vector weight of the same length
   and of stride w-stide specifies the weight of each datapoint. The
   weight is the reciprocal of the variance for each datapoint in y.
 
   The variance of the parameter c1 is computed using the weights
   and returned as the second value.  The weighted sum of
   squares of the residuals from the best-fit line, \chi^2, is
   returned as the last value.")

(defmfun multiplier-estimate (x c1 cov11)
  "gsl_fit_mul_est"
  ((x :double) (c1 :double) (cov11 :double)
   (y :double) (y-error :double))
  :documentation			; FDL
  "Use the best-fit linear regression coefficient
   c1 and its covariance cov11 to compute the fitted function
   y and its standard deviation y-error for the model
   Y = c_0 + c_1 X at the point x.")

;;;;****************************************************************************
;;;; Multiparameter fitting
;;;;****************************************************************************

(defmobject fit-workspace
    "gsl_multifit_linear"
  ((number-of-observations sizet) (number-of-parameters sizet))
  "multi-dimensional root solver with function only"
  "Make a workspace for a multidimensional linear least-squares fit.")

(defmfun linear-mfit
    (model observations parameters covariance tolerance workspace)
  "gsl_multifit_linear"
  (((mpointer model) :pointer) ((mpointer observations) :pointer)
   (tolerance :double)
   ((mpointer parameters) :pointer) (covariance :pointer) (chisq :double)
   ((mpointer workspace) :pointer))
  :inputs (model observations)
  :outputs (parameters covariance)
  :documentation			; FDL
  "Compute the best-fit parameters c of the model
   y = X c for the observations y and the matrix of predictor
   variables X.  The variance-covariance matrix of the model
   parameters cov is estimated from the scatter of the observations
   about the best-fit.  The sum of squares of the residuals from the
   best-fit, chi^2, is returned.

   The best-fit is found by singular value decomposition of the matrix
   X using the preallocated workspace provided. The
   modified Golub-Reinsch SVD algorithm is used, with column scaling to
   improve the accuracy of the singular values. Any components which have
   zero singular value (to machine precision) are discarded from the fit.")

(defmfun linear-mfit-svd
    (model observations parameters covariance tolerance workspace)
  "gsl_multifit_linear_svd"
  (((mpointer model) :pointer) ((mpointer observations) :pointer)
   (tolerance :double)
   (rank sizet)
   ((mpointer parameters) :pointer) (covariance :pointer) (chisq :double)
   ((mpointer workspace) :pointer))
  :inputs (model observations)
  :outputs (parameters covariance)
  :return ((dcref chisq) (scref rank))
  :documentation			; FDL
  "Compute the best-fit parameters c of the model
   y = X c for the observations y and the matrix of predictor
   variables X.  The variance-covariance matrix of the model
   parameters cov is estimated from the scatter of the observations
   about the best-fit.  The sum of squares of the residuals
   from the best-fit chi^2, and rank are returned.

   The best-fit is found by singular value decomposition of the matrix
   X using the preallocated workspace provided. The
   modified Golub-Reinsch SVD algorithm is used, with column scaling to
   improve the accuracy of the singular values. Any components which have
   zero singular value (to machine precision) are discarded from the fit.
   In the this form of the function the components are discarded if the
   ratio of singular values s_i/s_0 falls below the user-specified
   tolerance tolerance, and the effective rank is returned as the
   second value.")

(defmfun weighted-linear-mfit
    (model weight observations parameters covariance workspace)
  "gsl_multifit_wlinear"
  (((mpointer model) :pointer)
   ((mpointer weight) :pointer)
   ((mpointer observations) :pointer)
   ((mpointer parameters) :pointer)
   ((mpointer covariance) :pointer) (chisq :double)
   ((mpointer workspace) :pointer))
  :inputs (model observations)
  :outputs (parameters covariance)
  :documentation			; FDL
  "Compute the best-fit parameters c of the weighted
   model y = X c for the observations y and weights
   and the model matrix X.  The covariance matrix of
   the model parameters is computed with the given weights.  The
   weighted sum of squares of the residuals from the best-fit,
   chi^2, is returned as the last value.

   The best-fit is found by singular value decomposition of the matrix
   model using the preallocated workspace provided. Any
   components which have zero singular value (to machine precision) are
   discarded from the fit.")

(defmfun weighted-linear-mfit-svd
    (model weight observations parameters covariance tolerance workspace)
  "gsl_multifit_wlinear_svd"
  (((mpointer model) :pointer)
   ((mpointer weight) :pointer)
   ((mpointer observations) :pointer)
   (tolerance :double)
   (rank sizet)
   ((mpointer parameters) :pointer) (covariance :pointer) (chisq :double)
   ((mpointer workspace) :pointer))
  :inputs (model weight observations)
  :outputs (parameters covariance)
  :return ((dcref chisq) (scref rank))
  :documentation			; FDL
  "Compute the best-fit parameters c of the weighted
   model y = X c for the observations y and weights
   and the model matrix X.  The covariance matrix of
   the model parameters is computed with the given weights.  
   The weighted sum of squares of the residuals from the best-fit,
   chi^2, is returned as the first value.

   The best-fit is found by singular value decomposition of the matrix
   model using the preallocated workspace provided. Any
   components which have zero singular value (to machine precision) are
   discarded from the fit.  In the second form of the function the
   components are discarded if the ratio of singular values s_i/s_0
   falls below the user-specified tolerance, and the effective
   rank is returned as the second value.")

(defmfun multi-linear-estimate (x coefficients covariance)
  "gsl_multifit_linear_est"
  (((mpointer x) :pointer) ((mpointer coefficients) :pointer)
   ((mpointer covariance) :pointer) (y :double) (y-error :double))
  :inputs (x coefficients covariance)
  :documentation			; FDL
  "Use the best-fit multilinear regression coefficients
   and their covariance matrix to compute the fitted function value
   y and its standard deviation for the model y = x.c
   at the point x.")

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

(defun univariate-linear-least-squares-example ()
  "First example in Section 36.5 of the GSL manual."
  ;; Results not given in manual so not verified yet.
  (let ((x #m(1970.0d0 1980.0d0 1990.0d0 2000.0d0))
	 (y #m(12.0d0 11.0d0 14.0d0 13.0d0))
	 (w #m(0.1d0 0.2d0 0.3d0 0.4d0)))
	(multiple-value-bind (c0 c1 cov00 cov01 cov11 chisq)
	    (weighted-linear-fit x w y)
	  (format t "~&Best fit: Y = ~8,5f + ~8,5f X" c0 c1)
	  (format t "~&Covariance matrix:~&[~12,5f ~12,5f~&~12,5f ~12,5f]"
		  cov00 cov01 cov01 cov11)
	  (format t "~&Chisq = ~g" chisq)
	  (loop for i from 0 below (dim0 x)
		do
		(format t "~&data: ~12,5f ~12,5f ~12,5f"
			(maref x i)
			(maref y i)
			(/ (maref w i))))
	  (loop for i from -30 below 130 by 10 ; don't print everything
		for
		xf = (+ (maref x 0)
			(* (/ i 100)
			   (- (maref x (1- (dim0 x)))
			      (maref x 0))))
		do
		(multiple-value-bind (yf yferr)
		    (linear-estimate xf c0 c1 cov00 cov01 cov11)
		  (format t "~&fit:~6t~g ~g" xf yf)
		  (format t "~&high:~6t~g ~g" xf (+ yf yferr))
		  (format t "~&low:~6t~g ~g" xf (- yf yferr))))
	  (fresh-line))))

(defun mv-linear-least-squares-data ()
  "Generate data for second example in Section 36.5 of the GSL
   manual."
  (let ((rng (make-random-number-generator *mt19937* 0)))
    (loop for x from 1/10 below 2 by 1/10
	  for xd = (coerce x 'double-float)
	  for y0 = (exp xd)
	  for sigma = (* 0.1d0 y0)
	  collect
	  (list xd (+ y0 (gaussian rng sigma)) sigma))))

(defun mv-linear-least-squares-example (data)
  "Second example in Section 36.5 of the GSL manual."
  (let* ((n (length data)) chisq
	 (x (make-marray 'double-float :dimensions (list n 3)))
	 (cov (make-marray 'double-float :dimensions '(3 3)))
	 (y (make-marray 'double-float :dimensions n))
	 (w (make-marray 'double-float :dimensions n))
	 (c (make-marray 'double-float :dimensions 3)))
    (loop for i from 0
       for row in data do
       (setf (maref X i 0) 1.0d0
	     (maref X i 1) (first row)
	     (maref X i 2) (expt (first row) 2)
	     (maref y i) (second row)
	     (maref w i) (/ (expt (third row) 2))))
    (let ((ws (make-fit-workspace n 3)))
      (setf chisq
	    (weighted-linear-mfit X w y c cov ws)))
    (format t "~&Best fit: Y = ~10,8f + ~10,8f X + ~10,8f X^2"
	    (maref c 0) (maref c 1) (maref c 2))
    (format t "~&Covariance matrix:")
    (format
     t "~&~10,8f ~10,8f ~10,8f"
     (maref cov 0 0) (maref cov 0 1) (maref cov 0 2))
    (format
     t "~&~10,8f ~10,8f ~10,8f"
     (maref cov 1 0) (maref cov 1 1) (maref cov 1 2))
    (format
     t "~&~10,8f ~10,8f ~10,8f"
     (maref cov 2 0) (maref cov 2 1) (maref cov 2 2))
    (format t "~&Chisq = ~10,6f" chisq)))

;;; (mv-linear-least-squares-example (mv-linear-least-squares-data))
