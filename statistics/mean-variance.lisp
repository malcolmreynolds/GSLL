;; Mean, standard deviation, and variance    
;; Liam Healy, Sat Dec  2 2006 - 22:15
;; Time-stamp: <2008-09-21 15:52:16EDT mean-variance.lisp>
;; $Id$

(in-package :gsl)

;;; To do: stride other than 1 when that information is available from
;;; the vector.

;;;;****************************************************************************
;;;; Mean and weighted mean
;;;;****************************************************************************

(defmfun mean ((vector vector))
  ("gsl_stats" :type "_mean")
  (((c-pointer vector) :pointer) (1 :int) ((dim0 vector) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
   "The arithmetic mean of the vector.
   The arithmetic mean, or sample mean, is denoted by
   \Hat\mu and defined as \Hat\mu = (1/N) \sum x_i.  Returns a double-float.")

(defmfun weighted-mean ((vector vector) (weights vector))
  ("gsl_stats" :type "_wmean")
  (((c-pointer weights) :pointer) (1 :int)
   ((c-pointer vector) :pointer) (1 :int)
   ((dim0 vector) sizet))
  :definition :generic
  :element-types :float
  :c-return :element-c-type
  :documentation			; FDL
  "The weighted mean of the dataset, using the set of weights
    The weighted mean is defined as
    \Hat\mu = (\sum w_i x_i) / (\sum w_i).")

;;;;****************************************************************************
;;;; Variance
;;;;****************************************************************************

(defmfun variance ((vector vector) &optional mean)
  (("gsl_stats" :type "_variance")
   ("gsl_stats" :type "_variance_m"))
  ((((c-pointer vector) :pointer) (1 :int) ((dim0 vector) sizet))
   (((c-pointer vector) :pointer) (1 :int)
   ((dim0 vector) sizet) (mean :double)))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "The estimated, or sample, variance of data.  The
   estimated variance is denoted by \Hat\sigma^2 and is defined by
   \Hat\sigma^2 = (1/(N-1)) \sum (x_i - \Hat\mu)^2
   where x_i are the elements of the dataset data.  Note that
   the normalization factor of 1/(N-1) results from the derivation
   of \Hat\sigma^2 as an unbiased estimator of the population
   variance \sigma^2.  For samples drawn from a gaussian distribution
   the variance of \Hat\sigma^2 itself is 2 \sigma^4 / N.
   If the mean value is known, it may be supplied which will use more
   efficient routines to compute the variance.")

(defmfun weighted-variance ((vector vector) (weights vector) &optional mean)
  (("gsl_stats" :type "_wvariance")
   ("gsl_stats" :type "_wvariance_m"))
  ((((c-pointer weights) :pointer) (1 :int)
    ((c-pointer vector) :pointer) (1 :int)
    ((dim0 vector) sizet))
   (((c-pointer weights) :pointer) (1 :int)
    ((c-pointer vector) :pointer) (1 :int)
    ((dim0 vector) sizet) (mean :double)))
  :definition :generic
  :element-types :float
  :c-return :double
  :documentation			; FDL
  "The estimated variance of a weighted dataset is defined as
   \Hat\sigma^2 = ((\sum w_i)/((\sum w_i)^2 - \sum (w_i^2))) 
                \sum w_i (x_i - \Hat\mu)^2
   Note that this expression reduces to an unweighted variance with the
   familiar 1/(N-1) factor when there are N equal non-zero weights.
   If the mean value is known, it may be supplied which will use more
   efficient routines to compute the variance.")

;;;;****************************************************************************
;;;; Standard deviation
;;;;****************************************************************************

(defmfun standard-deviation ((vector vector) &optional mean)
  (("gsl_stats" :type "_sd")
   ("gsl_stats" :type "_sd_m"))
  ((((c-pointer vector) :pointer) (1 :int) ((dim0 vector) sizet))
   (((c-pointer vector) :pointer) (1 :int)
   ((dim0 vector) sizet) (mean :double)))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "The standard deviation, square root of the variance.
   If the mean value is known, it may be supplied which will use more
   efficient routines to compute the variance.")

(defmfun weighted-standard-deviation ((vector vector) (weights vector) &optional mean)
  (("gsl_stats" :type "_wsd")
   ("gsl_stats" :type "_wsd_m"))
  ((((c-pointer weights) :pointer) (1 :int)
    ((c-pointer vector) :pointer) (1 :int)
    ((dim0 vector) sizet))
   (((c-pointer weights) :pointer) (1 :int)
    ((c-pointer vector) :pointer) (1 :int)
    ((dim0 vector) sizet) (mean :double)))
  :definition :generic
  :element-types :float
  :c-return :double
  :documentation			; FDL
  "The weighted standard deviation, square root of the variance.
   If the mean value is known, it may be supplied which will use more
   efficient routines to compute the variance.")

;;;;****************************************************************************
;;;; With fixed mean
;;;;****************************************************************************

(defmfun variance-with-fixed-mean ((vector vector) mean)
  ("gsl_stats" :type "_variance_with_fixed_mean")
  (((c-pointer vector) :pointer) (1 :int)
   ((dim0 vector) sizet) (mean :double))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "An unbiased estimate of the variance of
    data when the population mean mean of the underlying
    distribution is known a priori.  In this case the estimator for
    the variance uses the factor 1/N and the sample mean
    \Hat\mu is replaced by the known population mean \mu,
    \Hat\sigma^2 = (1/N) \sum (x_i - \mu)^2.")

(defmfun standard-deviation-with-fixed-mean ((vector vector) mean)
  ("gsl_stats" :type "_sd_with_fixed_mean")
  (((c-pointer vector) :pointer) (1 :int)
   ((dim0 vector) sizet) (mean :double))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
   "The standard deviation of data for a fixed population
    mean.  The result is the square root of the
    corresponding variance function.")

;;;;****************************************************************************
;;;; Weighted with fixed mean
;;;;****************************************************************************

(defmfun weighted-variance-with-fixed-mean
    ((vector vector) (weights vector) mean)
  ("gsl_stats" :type "_wvariance_with_fixed_mean")
  (((c-pointer weights) :pointer) (1 :int)
   ((c-pointer vector) :pointer) (1 :int)
   ((dim0 vector) sizet) (mean :double))
  :definition :generic
  :element-types :float
  :c-return :double
  :documentation			; FDL
  "An unbiased estimate of the variance of weighted
    dataset when the population mean of the underlying
    distribution is known a priori.  In this case the estimator for
    the variance replaces the sample mean \Hat\mu by the known
    population mean \mu,
    \Hat\sigma^2 = (\sum w_i (x_i - \mu)^2) / (\sum w_i).")

(defmfun weighted-standard-deviation-with-fixed-mean
    ((vector vector) (weights vector) mean)
  ("gsl_stats" :type "_wsd_with_fixed_mean")
  (((c-pointer weights) :pointer) (1 :int)
   ((c-pointer vector) :pointer) (1 :int)
   ((dim0 vector) sizet) (mean :double))
  :definition :generic
  :element-types :float
  :c-return :double
  :documentation			; FDL
  "The square root of the corresponding variance
   function #'weighted-variance-with-fixed-mean.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests mean-variance
 (letm ((vec (vector-double-float (a -3.21d0 1.0d0 12.8d0)))
	(weights (vector-double-float (a 3.0d0 1.0d0 2.0d0))))
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
 (letm ((vec (vector-signed-byte-32 (a 8 4 -2))))
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
	((VEC (VECTOR-DOUBLE-FLOAT (A -3.21d0 1.0d0 12.8d0)))
	 (WEIGHTS (VECTOR-DOUBLE-FLOAT (A 3.0d0 1.0d0 2.0d0))))
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
    (LETM ((VEC (VECTOR-SIGNED-BYTE-32 (A 8 4 -2))))
      (LET ((MEAN (MEAN VEC)))
	(LIST MEAN (VARIANCE VEC) (VARIANCE VEC MEAN)
	      (STANDARD-DEVIATION VEC)
	      (STANDARD-DEVIATION VEC MEAN)
	      (VARIANCE-WITH-FIXED-MEAN VEC 4.0d0)
	      (STANDARD-DEVIATION-WITH-FIXED-MEAN VEC
						  4.0d0)))))))
