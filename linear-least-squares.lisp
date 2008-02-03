;; Linear least squares, or linear regression
;; Liam Healy <2008-01-21 12:41:46EST linear-least-squares.lisp>
;; Time-stamp: <2008-02-03 11:31:47EST linear-least-squares.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Linear regression
;;;;****************************************************************************

;;; Error in GSL documentation or in code: vectors must be equal
;;; lengths but strides re not?  Shouldn't (floor length stride) be n?
;;; Error in GSL documentation for gsl_fit_linear_est, "c00" instead
;;; of "cov00" etc.  Last arg to gsl_fit_wmul is labelled sumsq but
;;; referred to as chisq.

(defun-gsl linear-fit (x y &optional (x-stride 1) (y-stride 1))
  "gsl_fit_linear"
  (((gsl-array x) :pointer) (x-stride :size)
   ((gsl-array y) :pointer) (y-stride :size)
   ((dim0 x) :size) (c0 :double) (c1 :double)
   (cov00 :double) (cov01 :double) (cov11 :double)
   (sumsq :double))
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

(defun-gsl weighted-linear-fit
    (x weight y &optional (x-stride 1) (weight-stride 1) (y-stride 1))
  "gsl_fit_wlinear"
  (((gsl-array x) :pointer) (x-stride :size)
   ((gsl-array weight) :pointer) (weight-stride :size)
   ((gsl-array y) :pointer) (y-stride :size)
   ((dim0 x) :size) (c0 :double) (c1 :double)
   (cov00 :double) (cov01 :double) (cov11 :double)
   (chisq :double))
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

(defun-gsl linear-estimate (x c0 c1 cov00 cov01 cov11)
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

(defun-gsl multiplier-fit (x y &optional (x-stride 1) (y-stride 1))
  "gsl_fit_mul"
  (((gsl-array x) :pointer) (x-stride :size)
   ((gsl-array y) :pointer) (y-stride :size)
   ((dim0 x) :size) (c1 :double) (cov11 :double)
   (sumsq :double))
  :documentation			; FDL
  "The best-fit linear regression coefficient c1 of the model Y = c_1
   X for the datasets (x, y) two vectors of equal length with strides x-stride
   and y-stride.  The errors on y are assumed unknown so the variance of
   the parameter c1 is estimated from the scatter of the points around
   the best-fit line and returned as the the second value.  The sum of
   squares of the residuals from the best-fit line is returned as the
   last value.")

(defun-gsl weighted-multiplier-fit
    (x weight y &optional (x-stride 1) (weight-stride 1) (y-stride 1))
  "gsl_fit_wmul"
  (((gsl-array x) :pointer) (x-stride :size)
   ((gsl-array weight) :pointer) (weight-stride :size)
   ((gsl-array y) :pointer) (y-stride :size)
   ((dim0 x) :size) (c1 :double) (cov11 :double)
   (chisq :double))
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

(defun-gsl multiplier-estimate (x c1 cov11)
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

(set-asf (fit-workspace number-of-observations number-of-parameters)
	 allocate-fit-workspace free-fit-workspace nil 2)

(defun-gsl allocate-fit-workspace (number-of-observations number-of-parameters)
  "gsl_multifit_linear_alloc"
  ((number-of-observations :size) (number-of-parameters :size))
  :c-return :pointer
  :index (letm fit-workspace)
  :documentation			; FDL
  "Allocate a workspace for fitting a linear model.")

(defun-gsl free-fit-workspace (pointer)
  "gsl_multifit_linear_free"
  ((pointer :pointer))
  :c-return :void
  :index (letm fit-workspace)
  :documentation			; FDL
  "Free the memory associate with the workspace.")

(defun-gsl linear-mfit
    (model observations parameters covariance tolerance workspace)
  "gsl_multifit_linear"
  (((pointer model) :pointer) ((pointer observations) :pointer)
   (tolerance :double)
   ((pointer parameters) :pointer) (covariance :pointer) (chisq :double)
   (workspace :pointer))
  :documentation			; FDL
  "Compute the best-fit parameters c of the model
   y = X c for the observations y and the matrix of predictor
   variables X.  The variance-covariance matrix of the model
   parameters cov is estimated from the scatter of the observations
   about the best-fit.  The sum of squares of the residuals from the
   best-fit, \chi^2, is returned.

   The best-fit is found by singular value decomposition of the matrix
   X using the preallocated workspace provided. The
   modified Golub-Reinsch SVD algorithm is used, with column scaling to
   improve the accuracy of the singular values. Any components which have
   zero singular value (to machine precision) are discarded from the fit.")

(defun-gsl linear-mfit-svd
    (model observations parameters covariance tolerance workspace)
  "gsl_multifit_linear_svd"
  (((pointer model) :pointer) ((pointer observations) :pointer)
   (tolerance :double)
   ((pointer parameters) :pointer) (covariance :pointer) (chisq :double)
   (workspace :pointer))
  :documentation			; FDL
  "Compute the best-fit parameters c of the model
   y = X c for the observations y and the matrix of predictor
   variables X.  The variance-covariance matrix of the model
   parameters cov is estimated from the scatter of the observations
   about the best-fit.  The sum of squares of the residuals from the
   best-fit, \chi^2, is returned.

   The best-fit is found by singular value decomposition of the matrix
   X using the preallocated workspace provided. The
   modified Golub-Reinsch SVD algorithm is used, with column scaling to
   improve the accuracy of the singular values. Any components which have
   zero singular value (to machine precision) are discarded from the fit.
   In the this form of the function the components are discarded if the
   ratio of singular values @math{s_i/s_0} falls below the user-specified
   tolerance @var{tol}, and the effective rank is returned in @var{rank}.")

(defun-gsl weighted-linear-mfit
    (model weight observations parameters covariance workspace)
  "gsl_multifit_wlinear"
  (((pointer model) :pointer)
   ((pointer weight) :pointer)
   ((pointer observations) :pointer)
   ((pointer parameters) :pointer)
   ((pointer covariance) :pointer) (chisq :double)
   (workspace :pointer))
  :documentation			; FDL
  "Compute the best-fit parameters c of the weighted
   model y = X c for the observations y and weights
   and the model matrix X.  The covariance matrix of
   the model parameters is computed with the given weights.  The
   weighted sum of squares of the residuals from the best-fit,
   \chi^2, is returned as the last value.

   The best-fit is found by singular value decomposition of the matrix
   model using the preallocated workspace provided. Any
   components which have zero singular value (to machine precision) are
   discarded from the fit.")

(defun-gsl weighted-linear-mfit-svd
    (model weight observations parameters covariance tolerance workspace)
  "gsl_multifit_wlinear_svd"
  (((pointer model) :pointer)
   ((pointer weight) :pointer)
   ((pointer observations) :pointer)
   (tolerance :double)
   (rank :size)
   ((pointer parameters) :pointer) (covariance :pointer) (chisq :double)
   (workspace :pointer))
  :return ((dcref chisq) (scref rank))
  :documentation			; FDL
  "Compute the best-fit parameters c of the weighted
   model y = X c for the observations y and weights
   and the model matrix X.  The covariance matrix of
   the model parameters is computed with the given weights.  The
   weighted sum of squares of the residuals from the best-fit,
   \chi^2, is returned as the first value.

   The best-fit is found by singular value decomposition of the matrix
   model using the preallocated workspace provided. Any
   components which have zero singular value (to machine precision) are
   discarded from the fit.  In the second form of the function the
   components are discarded if the ratio of singular values s_i/s_0
   falls below the user-specified tolerance, and the effective
   rank is returned as the second value.")

(defun-gsl multi-linear-estimate (x coefficients covariance)
  "gsl_multifit_linear_est"
  ((x :double) (coefficients :pointer)
   (covariance :pointer) (y :double) (y-error :double))
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
  (with-data (x vector-double 4)
    (with-data (y vector-double 4)
      (with-data (w vector-double 4)
	(setf (data x) #(1970.0d0 1980.0d0 1990.0d0 2000.0d0)
	      (data y) #(12.0d0 11.0d0 14.0d0 13.0d0)
	      (data w) #(0.1d0 0.2d0 0.3d0 0.4d0))
	(multiple-value-bind (c0 c1 cov00 cov01 cov11 chisq)
	    (weighted-linear-fit x w y)
	  (format t "~&Best fit: Y = ~8,5f + ~8,5f X" c0 c1)
	  (format t "~&Covariance matrix:~&[~12,5f ~12,5f~&~12,5f ~12,5f]"
		  cov00 cov01 cov01 cov11)
	  (format t "~&Chisq = ~g" chisq)
	  (loop for i from 0 below (dim0 x)
		do
		(format t "~&data: ~12,5f ~12,5f ~12,5f"
			(gsl-aref x i)
			(gsl-aref y i)
			(/ (gsl-aref w i))))
	  (loop for i from -30 below 130 by 10 ; don't print everything
		for
		xf = (+ (gsl-aref x 0)
			(* (/ i 100)
			   (- (gsl-aref x (1- (dim0 x)))
			      (gsl-aref x 0))))
		do
		(multiple-value-bind (yf yferr)
		    (linear-estimate xf c0 c1 cov00 cov01 cov11)
		  (format t "~&fit:~6t~g ~g" xf yf)
		  (format t "~&high:~6t~g ~g" xf (+ yf yferr))
		  (format t "~&low:~6t~g ~g" xf (- yf yferr))))
	  (fresh-line))))))

(defun mv-linear-least-squares-data ()
  "Generate data for second example in Section 36.5 of the GSL
   manual."
  (letm ((rng (random-number-generator *mt19937* 0)))
    (loop for x from 1/10 below 2 by 1/10
	  for xd = (coerce x 'double-float)
	  for y0 = (exp xd)
	  for sigma = (* 0.1d0 y0)
	  collect
	  (list xd (+ y0 (gaussian rng sigma)) sigma))))

(defun mv-linear-least-squares-example (data)
  "Second example in Section 36.5 of the GSL manual."
  (let ((n (length data)) chisq)
    (with-data (x matrix-double (n 3))
      (with-data (cov matrix-double (3 3))
	(with-data (y vector-double n)
	  (with-data (w vector-double n)
	    (with-data (c vector-double 3)
	      (loop for i from 0
		    for row in data do
		    (setf (gsl-aref X i 0) 1.0d0
			  (gsl-aref X i 1) (first row)
			  (gsl-aref X i 2) (expt (first row) 2)
			  (gsl-aref y i) (second row)
			  (gsl-aref w i) (/ (expt (third row) 2))))
	      (letm ((ws (fit-workspace n 3)))
		(setf chisq
		      (weighted-linear-mfit X w y c cov ws)))
	      (format t "~&Best fit: Y = ~10,8f + ~10,8f X + ~10,8f X^2"
		      (gsl-aref c 0) (gsl-aref c 1) (gsl-aref c 2))
	      (format t "~&Covariance matrix:")
	      (format
	       t "~&~10,8f ~10,8f ~10,8f"
	       (gsl-aref cov 0 0) (gsl-aref cov 0 1) (gsl-aref cov 0 2))
	      (format
	       t "~&~10,8f ~10,8f ~10,8f"
	       (gsl-aref cov 1 0) (gsl-aref cov 1 1) (gsl-aref cov 1 2))
	      (format
	       t "~&~10,8f ~10,8f ~10,8f"
	       (gsl-aref cov 2 0) (gsl-aref cov 2 1) (gsl-aref cov 2 2))
	      (format t "~&Chisq = ~10,6f" chisq))))))))

;;; (mv-linear-least-squares-example (mv-linear-least-squares-data))
