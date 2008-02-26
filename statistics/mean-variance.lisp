;; Mean, standard deviation, and variance    
;; Liam Healy, Sat Dec  2 2006 - 22:15
;; Time-stamp: <2008-02-17 16:35:48EST mean-variance.lisp>
;; $Id$

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmacro defmfun-stats (&rest args)
  "A defmfun for stats of double, single, and fixnum."
  (defmfun-all
      ;;'(double single fixnum long)
      '(double single fixnum)
      ;;'(:double :float :int :long-double)
      '(:double :float :int)
    "stats"
    'gsl-vector
    args))

(defmacro defmfun-stats-ds (&rest args)
  "A defmfun for stats of double and single."
  (defmfun-all
      '(double single)
      '(:double :float)
    "stats"
    'gsl-vector
    args))

;;;;****************************************************************************
;;;; Mean and weighted mean
;;;;****************************************************************************

(defgeneric mean (gsl-vector)
  (:documentation			; FDL
   "The arithmetic mean of the vector.
   The arithmetic mean, or sample mean, is denoted by
   \Hat\mu and defined as \Hat\mu = (1/N) \sum x_i.  Returns a double-float."))

(defmfun-stats mean ((vector gsl-vector))
  "gsl_stats_mean"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size))
  :c-return :double)

(defgeneric weighted-mean (gsl-vector weights)
  (:documentation			; FDL
   "The weighted mean of the dataset, using the set of weights
    The weighted mean is defined as
    \Hat\mu = (\sum w_i x_i) / (\sum w_i)."))

(defmfun-stats-ds weighted-mean ((vector gsl-vector) weights)
  "gsl_stats_wmean"
  (((gsl-array weights) :pointer) (1 :int)
    ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size))
  :c-return :double)

;;;;****************************************************************************
;;;; Variance
;;;;****************************************************************************

(defgeneric variance-nom (gsl-vector)
  (:documentation			; FDL
   "The estimated, or sample, variance of data.  The
   estimated variance is denoted by \Hat\sigma^2 and is defined by
   \Hat\sigma^2 = (1/(N-1)) \sum (x_i - \Hat\mu)^2
   where x_i are the elements of the dataset data.  Note that
   the normalization factor of 1/(N-1) results from the derivation
   of \Hat\sigma^2 as an unbiased estimator of the population
   variance \sigma^2.  For samples drawn from a gaussian distribution
   the variance of \Hat\sigma^2 itself is 2 \sigma^4 / N.

   This function computes the mean via a call to #'mean.  If
   you have already computed the mean then you can pass it directly to
   #'variance-m."))

(defmfun-stats variance-nom ((vector gsl-vector))
  "gsl_stats_variance"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size))
  :index variance
  :export nil
  :c-return :double)

(defgeneric variance-m (gsl-vector mean)
  (:documentation			; FDL
   "Compute the variance with the mean known to
   avoid its recomputation."))

(defmfun-stats variance-m ((vector gsl-vector) mean)
  "gsl_stats_variance_m"
  (((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) size) (mean :double))
  :index variance
  :export nil
  :c-return :double)

(export 'variance)
(defun-optionals variance (data &optional mean)
  -nom -m
  ;; FDL
  "The estimated, or sample, variance of data.  The
   estimated variance is denoted by \Hat\sigma^2 and is defined by
   \Hat\sigma^2 = (1/(N-1)) \sum (x_i - \Hat\mu)^2
   where x_i are the elements of the dataset.  Note that
   the normalization factor of 1/(N-1) results from the derivation
   of \Hat\sigma^2 as an unbiased estimator of the population
   variance \sigma^2.  For samples drawn from a gaussian distribution
   the variance of \Hat\sigma^2 itself is 2 \sigma^4 / N.")

;;;;****************************************************************************
;;;; Weighted variance
;;;;****************************************************************************

(defgeneric weighted-variance-nom (gsl-vector weights)
  (:documentation			; FDL
   "Compute the weighted variance with the mean unknown."))

(defmfun-stats-ds weighted-variance-nom ((vector gsl-vector) weights)
  "gsl_stats_wvariance"
  (((gsl-array weights) :pointer) (1 :int)
    ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size))
  :index weighted-variance
  :export nil
  :c-return :double)

(defgeneric weighted-variance-m (gsl-vector weights mean)
  (:documentation			; FDL
   "Compute the weighted variance with the mean known to
   avoid its recomputation."))

(defmfun-stats-ds weighted-variance-m ((vector gsl-vector) weights mean)
  "gsl_stats_wvariance_m"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) size) (mean :double))
  :index variance
  :export nil
  :c-return :double)

(export 'weighted-variance)
(defun-optionals weighted-variance (data weights &optional mean)
  -nom -m
  ;; FDL
   "The estimated variance using weights
   The estimated variance of a weighted dataset is defined as
   \Hat\sigma^2 = ((\sum w_i)/((\sum w_i)^2 - \sum (w_i^2))) 
                \sum w_i (x_i - \Hat\mu)^2
   Note that this expression reduces to an unweighted variance with the
   familiar 1/(N-1) factor when there are N equal non-zero weights.")

;;;;****************************************************************************
;;;; Standard deviation
;;;;****************************************************************************

(defgeneric standard-deviation-nom (gsl-vector)
  (:documentation			; FDL
   "The standard deviation, square root of the variance."))

(defmfun-stats standard-deviation-nom ((vector gsl-vector))
  "gsl_stats_sd"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size))
  :index standard-deviation
  :export nil
  :c-return :double)

(defgeneric standard-deviation-m (gsl-vector mean)
  (:documentation			; FDL
   "The standard deviation with the mean known to
   avoid its recomputation."))

(defmfun-stats standard-deviation-m ((vector gsl-vector) mean)
  "gsl_stats_sd_m"
  (((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) size) (mean :double))
  :c-return :double)

(export 'standard-deviation)
(defun-optionals standard-deviation (data &optional mean)
  -nom -m
  ;; FDL
  "The standard deviation, square root of the variance.")

;;;;****************************************************************************
;;;; Weighted standard deviation
;;;;****************************************************************************

(defgeneric weighted-standard-deviation-nom (gsl-vector weights))

(defmfun-stats-ds weighted-standard-deviation-nom
    ((vector gsl-vector) weights)
  "gsl_stats_wsd"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size))
  :index weighted-standard-deviation
  :export nil
  :c-return :double)

(defgeneric weighted-standard-deviation-m (gsl-vector weights mean))

(defmfun-stats-ds weighted-standard-deviation-m
    ((vector gsl-vector) weights mean)
  "gsl_stats_wsd_m"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) size) (mean :double))
  :c-return :double)

(export 'weighted-standard-deviation)
(defun-optionals weighted-standard-deviation (data weights &optional mean)
  -nom -m
  ;; FDL
  "The standard deviation, square root of the variance.")

;;;;****************************************************************************
;;;; Variance with fixed mean
;;;;****************************************************************************

(defgeneric variance-with-fixed-mean (gsl-vector mean)
  (:documentation			; FDL
   "An unbiased estimate of the variance of
    data when the population mean mean of the underlying
    distribution is known a priori.  In this case the estimator for
    the variance uses the factor 1/N and the sample mean
    \Hat\mu is replaced by the known population mean \mu,
    \Hat\sigma^2 = (1/N) \sum (x_i - \mu)^2."))

(defmfun-stats variance-with-fixed-mean ((vector gsl-vector) mean)
  "gsl_stats_variance_with_fixed_mean"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size)
   (mean :double))
  :c-return :double)

(defgeneric standard-deviation-with-fixed-mean (gsl-vector mean)
  (:documentation			; FDL
   "The standard deviation of data for a fixed population
    mean.  The result is the square root of the
    corresponding variance function."))

(defmfun-stats standard-deviation-with-fixed-mean
    ((vector gsl-vector) mean)
  "gsl_stats_sd_with_fixed_mean"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size)
   (mean :double))
  :c-return :double)

;;;;****************************************************************************
;;;; Weighted variance with fixed mean
;;;;****************************************************************************

(defgeneric weighted-variance-with-fixed-mean (gsl-vector weights mean)
  (:documentation			; FDL
   "An unbiased estimate of the variance of weighted
    dataset when the population mean of the underlying
    distribution is known a priori.  In this case the estimator for
    the variance replaces the sample mean \Hat\mu by the known
    population mean \mu,
    \Hat\sigma^2 = (\sum w_i (x_i - \mu)^2) / (\sum w_i)."))

(defmfun-stats-ds weighted-variance-with-fixed-mean
    ((vector gsl-vector) weights mean)
  "gsl_stats_wvariance_with_fixed_mean"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size)
   (mean :double))
  :c-return :double)

(defgeneric weighted-standard-deviation-with-fixed-mean
    (gsl-vector weights mean)
  (:documentation			; FDL
   "The square root of the corresponding variance
   function #'weighted-variance-with-fixed-mean."))

(defmfun-stats-ds weighted-standard-deviation-with-fixed-mean
    ((vector gsl-vector) weights mean)
  "gsl_stats_wsd_with_fixed_mean"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) size)
   (mean :double))
  :c-return :double)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests mean-variance
  (letm ((vec (vector-double #(-3.21d0 1.0d0 12.8d0)))
	   (weights (vector-double #(3.0d0 1.0d0 2.0d0))))
      (let ((mean (mean vec))
	    (wmean (weighted-mean vec weights)))
	(list
	 mean wmean
	 (variance vec)
	 (variance vec mean)
	 (weighted-variance vec weights)
	 (weighted-variance vec weights wmean)
	 (standard-deviation vec)
	 (standard-deviation vec mean)
	 (variance-with-fixed-mean vec 4.0d0)
	 (standard-deviation-with-fixed-mean vec 4.0d0))))
  (letm ((vec (vector-fixnum #(8 4 -2))))
      (let ((mean (mean vec)))
	(list
	 mean
	 (variance vec)
	 (variance vec mean)
	 (standard-deviation vec)
	 (standard-deviation vec mean)
	 (variance-with-fixed-mean vec 4.0d0)
	 (standard-deviation-with-fixed-mean vec 4.0d0)))))
|#

(LISP-UNIT:DEFINE-TEST MEAN-VARIANCE
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 3.5300000000000002d0 2.8283333333333336d0
	  68.88069999999999d0 68.88069999999999d0
	  84.98058636363639d0 84.98058636363639d0
	  8.29943974012704d0 8.29943974012704d0
	  46.14136666666667d0 6.792743677385941d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((VEC (VECTOR-DOUBLE #(-3.21d0 1.0d0 12.8d0)))
	 (WEIGHTS (VECTOR-DOUBLE #(3.0d0 1.0d0 2.0d0))))
      (LET ((MEAN (MEAN VEC))
	    (WMEAN (WEIGHTED-MEAN VEC WEIGHTS)))
	(LIST MEAN WMEAN (VARIANCE VEC) (VARIANCE VEC MEAN)
	      (WEIGHTED-VARIANCE VEC WEIGHTS)
	      (WEIGHTED-VARIANCE VEC WEIGHTS WMEAN)
	      (STANDARD-DEVIATION VEC)
	      (STANDARD-DEVIATION VEC MEAN)
	      (VARIANCE-WITH-FIXED-MEAN VEC 4.0d0)
	      (STANDARD-DEVIATION-WITH-FIXED-MEAN VEC
						  4.0d0))))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 3.3333333333333335d0 25.333333333333336d0
	  25.333333333333336d0 5.033222956847167d0
	  5.033222956847167d0 17.333333333333332d0
	  4.163331998932265d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-FIXNUM #(8 4 -2))))
      (LET ((MEAN (MEAN VEC)))
	(LIST MEAN (VARIANCE VEC) (VARIANCE VEC MEAN)
	      (STANDARD-DEVIATION VEC)
	      (STANDARD-DEVIATION VEC MEAN)
	      (VARIANCE-WITH-FIXED-MEAN VEC 4.0d0)
	      (STANDARD-DEVIATION-WITH-FIXED-MEAN VEC
						  4.0d0)))))))
