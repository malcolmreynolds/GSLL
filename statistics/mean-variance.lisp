;********************************************************
; file:        mean-variance.lisp                        
; description: Mean, standard deviation, and variance    
; date:        Sat Dec  2 2006 - 22:15                   
; author:      Liam M. Healy                             
; modified:    Sun Dec 31 2006 - 21:45
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmacro defun-gsl-stats (&rest args)
  "A defun-gsl for stats of double, single, and fixnum."
  (defun-gsl-all
      ;;'(double single fixnum long)
      '(double single fixnum)
      ;;'(:double :float :int :long-double)
      '(:double :float :int)
    "stats"
    'gsl-vector
    args))

;;;;****************************************************************************
;;;; Mean and weighted mean
;;;;****************************************************************************

(defgeneric mean (gsl-vector)
  (:documentation "The arithmetic mean of the vector.
   The arithmetic mean, or sample mean, is denoted by
   @math{\Hat\mu} and defined as
   \Hat\mu = (1/N) \sum x_i.  Returns a double-float."))

(defun-gsl-stats mean ((vector gsl-vector))
  "gsl_stats_mean"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :c-return :double)

(defgeneric weighted-mean (gsl-vector weights)
  (:documentation
   "The weighted mean of the dataset, using the set of weights
    The weighted mean is defined as
    \Hat\mu = (\sum w_i x_i) / (\sum w_i)."))

(defun-gsl-stats weighted-mean ((vector gsl-vector) weights)
  "gsl_stats_wmean"
  (((gsl-array weights) :pointer) (1 :int)
    ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :c-return :double)

;;;;****************************************************************************
;;;; Variance
;;;;****************************************************************************

(defgeneric variance-nom (gsl-vector)
  (:documentation "The estimated, or @dfn{sample}, variance of
   @var{data}, a dataset of length @var{n} with stride @var{stride}.  The
   estimated variance is denoted by @math{\Hat\sigma^2} and is defined by
   \Hat\sigma^2 = (1/(N-1)) \sum (x_i - \Hat\mu)^2
   where @math{x_i} are the elements of the dataset @var{data}.  Note that
   the normalization factor of @math{1/(N-1)} results from the derivation
   of @math{\Hat\sigma^2} as an unbiased estimator of the population
   variance @math{\sigma^2}.  For samples drawn from a gaussian distribution
   the variance of @math{\Hat\sigma^2} itself is @math{2 \sigma^4 / N}.

   This function computes the mean via a call to #'mean.  If
   you have already computed the mean then you can pass it directly to
   #'variance-m."))

(defun-gsl-stats variance-nom ((vector gsl-vector))
  "gsl_stats_variance"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :index variance
  :export nil
  :c-return :double)

(defgeneric variance-m (gsl-vector mean)
  (:documentation "Compute the variance with the mean known to
   avoid its recomputation."))

(defun-gsl-stats variance-m ((vector gsl-vector) mean)
  "gsl_stats_variance_m"
  (((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) :size) (mean :double))
  :index variance
  :export nil
  :c-return :double)

(export 'variance)
(defun-optionals variance (data &optional mean)
  -nom -m
  "The estimated, or @dfn{sample}, variance of
   @var{data}, a dataset of length @var{n} with stride @var{stride}.  The
   estimated variance is denoted by @math{\Hat\sigma^2} and is defined by
   \Hat\sigma^2 = (1/(N-1)) \sum (x_i - \Hat\mu)^2
   where @math{x_i} are the elements of the dataset @var{data}.  Note that
   the normalization factor of @math{1/(N-1)} results from the derivation
   of @math{\Hat\sigma^2} as an unbiased estimator of the population
   variance @math{\sigma^2}.  For samples drawn from a gaussian distribution
   the variance of @math{\Hat\sigma^2} itself is @math{2 \sigma^4 / N}.")

;;;;****************************************************************************
;;;; Weighted variance
;;;;****************************************************************************

(defgeneric weighted-variance-nom (gsl-vector weights)
   (:documentation nil))

(defun-gsl-stats weighted-variance-nom ((vector gsl-vector) weights)
  "gsl_stats_wvariance"
  (((gsl-array weights) :pointer) (1 :int)
    ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :index weighted-variance
  :export nil
  :c-return :double)

(defgeneric weighted-variance-m (gsl-vector weights mean)
  (:documentation "Compute the weighted variance with the mean known to
   avoid its recomputation."))

(defun-gsl-stats weighted-variance-m ((vector gsl-vector) weights mean)
  "gsl_stats_wvariance_m"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) :size) (mean :double))
  :index variance
  :export nil
  :c-return :double)

(export 'weighted-variance)
(defun-optionals weighted-variance (data weights &optional mean)
  -nom -m
   "The estimated variance using weights
   The estimated variance of a weighted dataset is defined as
   \Hat\sigma^2 = ((\sum w_i)/((\sum w_i)^2 - \sum (w_i^2))) 
                \sum w_i (x_i - \Hat\mu)^2
   Note that this expression reduces to an unweighted variance with the
   familiar @math{1/(N-1)} factor when there are @math{N} equal non-zero
   weights.")

;;;;****************************************************************************
;;;; Standard deviation
;;;;****************************************************************************

(defgeneric standard-deviation-nom (gsl-vector)
  (:documentation "The standard deviation, square root of the variance."))

(defun-gsl-stats standard-deviation-nom ((vector gsl-vector))
  "gsl_stats_sd"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :index standard-deviation
  :export nil
  :c-return :double)

(defgeneric standard-deviation-m (gsl-vector mean)
  (:documentation "The standard deviation with the mean known to
   avoid its recomputation."))

(defun-gsl-stats standard-deviation-m ((vector gsl-vector) mean)
  "gsl_stats_sd_m"
  (((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) :size) (mean :double))
  :c-return :double)

(export 'standard-deviation)
(defun-optionals standard-deviation (data &optional mean)
  -nom -m
  "The standard deviation, square root of the variance.")

;;;;****************************************************************************
;;;; Weighted standard deviation
;;;;****************************************************************************

(defgeneric weighted-standard-deviation-nom (gsl-vector weights))

(defun-gsl-stats weighted-standard-deviation-nom
    ((vector gsl-vector) weights)
  "gsl_stats_wsd"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :index weighted-standard-deviation
  :export nil
  :c-return :double)

(defgeneric weighted-standard-deviation-m (gsl-vector weights mean))

(defun-gsl-stats weighted-standard-deviation-m
    ((vector gsl-vector) weights mean)
  "gsl_stats_wsd_m"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) :size) (mean :double))
  :c-return :double)

(export 'weighted-standard-deviation)
(defun-optionals weighted-standard-deviation (data weights &optional mean)
  -nom -m
  "The standard deviation, square root of the variance.")

;;;;****************************************************************************
;;;; Variance with fixed mean
;;;;****************************************************************************

(defgeneric variance-with-fixed-mean (gsl-vector mean)
  (:documentation
   "An unbiased estimate of the variance of
    @var{data} when the population mean @var{mean} of the underlying
    distribution is known @emph{a priori}.  In this case the estimator for
    the variance uses the factor @math{1/N} and the sample mean
    @math{\Hat\mu} is replaced by the known population mean @math{\mu},
    \Hat\sigma^2 = (1/N) \sum (x_i - \mu)^2."))

(defun-gsl-stats variance-with-fixed-mean ((vector gsl-vector) mean)
  "gsl_stats_variance_with_fixed_mean"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size)
   (mean :double))
  :c-return :double)

(defgeneric standard-deviation-with-fixed-mean (gsl-vector mean)
  (:documentation
   "The standard deviation of @var{data} for a fixed population
    mean @var{mean}.  The result is the square root of the
    corresponding variance function."))

(defun-gsl-stats standard-deviation-with-fixed-mean
    ((vector gsl-vector) mean)
  "gsl_stats_sd_with_fixed_mean"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size)
   (mean :double))
  :c-return :double)

;;;;****************************************************************************
;;;; Weighted variance with fixed mean
;;;;****************************************************************************

(defgeneric weighted-variance-with-fixed-mean (gsl-vector weights mean)
  (:documentation
   "An unbiased estimate of the variance of weighted
    dataset when the population mean @var{mean} of the underlying
    distribution is known @emph{a priori}.  In this case the estimator for
    the variance replaces the sample mean @math{\Hat\mu} by the known
    population mean @math{\mu},
    \Hat\sigma^2 = (\sum w_i (x_i - \mu)^2) / (\sum w_i)."))

(defun-gsl-stats weighted-variance-with-fixed-mean
    ((vector gsl-vector) weights mean)
  "gsl_stats_wvariance_with_fixed_mean"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size)
   (mean :double))
  :c-return :double)

(defgeneric weighted-standard-deviation-with-fixed-mean
    (gsl-vector weights mean)
  (:documentation
   "The square root of the corresponding variance
   function #'weighted-variance-with-fixed-mean."))

(defun-gsl-stats weighted-standard-deviation-with-fixed-mean
    ((vector gsl-vector) weights mean)
  "gsl_stats_wsd_with_fixed_mean"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size)
   (mean :double))
  :c-return :double)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test mean-variance
  (lisp-unit:assert-equal
   '("0.353000000000d+01" "0.282833333333d+01" "0.688807000000d+02"
     "0.688807000000d+02" "0.849805863636d+02" "0.849805863636d+02"
     "0.829943974013d+01" "0.829943974013d+01" "0.461413666667d+02"
     "0.679274367739d+01")
   (with-data (vec vector-double 3)
     (setf (data vec) #(-3.21d0 1.0d0 12.8d0))
     (with-data (weights vector-double 3)
       (setf (data weights) #(3.0d0 1.0d0 2.0d0))
       (let ((mean (mean vec))
	     (wmean (weighted-mean vec weights)))
	 (lisp-unit:fp-sequence
	  (list
	   mean wmean
	   (variance vec)
	   (variance vec mean)
	   (weighted-variance vec weights)
	   (weighted-variance vec weights wmean)
	   (standard-deviation vec)
	   (standard-deviation vec mean)
	   (variance-with-fixed-mean vec 4.0d0)
	   (standard-deviation-with-fixed-mean vec 4.0d0)))))))
  (lisp-unit:assert-equal
   '("0.333333333333d+01" "0.253333333333d+02" "0.253333333333d+02"
     "0.503322295685d+01" "0.503322295685d+01" "0.173333333333d+02"
     "0.416333199893d+01")
   (with-data (vec vector-fixnum 3)
     (setf (data vec) #(8 4 -2))
     (let ((mean (mean vec)))
       (lisp-unit:fp-sequence
	(list
	 mean
	 (variance vec)
	 (variance vec mean)
	 (standard-deviation vec)
	 (standard-deviation vec mean)
	 (variance-with-fixed-mean vec 4.0d0)
	 (standard-deviation-with-fixed-mean vec 4.0d0)))))))
