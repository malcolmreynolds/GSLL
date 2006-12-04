;********************************************************
; file:        mean-variance.lisp                        
; description: Mean, standard deviation, and variance    
; date:        Sat Dec  2 2006 - 22:15                   
; author:      Liam M. Healy                             
; modified:    Sun Dec  3 2006 - 18:18
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmacro defun-gsl-stats (&rest args)
  "A defun-gsl for stats of double, single, and fixnum."
  (defun-gsl-all
      '(double single fixnum)
      '(:double :float :int)
    "stats"
    'gsl-vector
    args))

(defgeneric mean (gsl-vector)
  (:documentation "The arithmetic mean of the vector.
   The arithmetic mean, or sample mean, is denoted by
   @math{\Hat\mu} and defined as
   \Hat\mu = (1/N) \sum x_i.  Returns a double-float."))

(defun-gsl-stats mean ((vector gsl-vector))
  "gsl_stats_mean"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :c-return :double)

(defgeneric variance (gsl-vector)
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
   #'variance-m"))

(defun-gsl-stats variance ((vector gsl-vector))
  "gsl_stats_variance"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :c-return :double)

(defgeneric variance-m (gsl-vector mean)
  (:documentation "Compute the variance with the mean known to
   avoid its recomputation."))

(defun-gsl-stats variance-m ((vector gsl-vector) mean)
  "gsl_stats_variance_m"
  (((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) :size) (mean :double))
  :c-return :double)

(defgeneric standard-deviation (gsl-vector)
  (:documentation "The standard deviation, square root of the variance."))

(defun-gsl-stats standard-deviation ((vector gsl-vector))
  "gsl_stats_sd"
  (((gsl-array vector) :pointer) (1 :int) ((dim0 vector) :size))
  :c-return :double)

(defgeneric standard-deviation-m (gsl-vector mean)
  (:documentation "The standard deviation with the mean known to
   avoid its recomputation."))

(defun-gsl-stats standard-deviation-m ((vector gsl-vector) mean)
  "gsl_stats_sd_m"
  (((gsl-array vector) :pointer) (1 :int)
   ((dim0 vector) :size) (mean :double))
  :c-return :double)

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

;;; Examples and unit test
(lisp-unit:define-test mean-variance
  (lisp-unit:assert-equal
   '("0.353000000000d+01" "0.688807000000d+02" "0.688807000000d+02"
     "0.829943974013d+01" "0.829943974013d+01" "0.461413666667d+02"
     "0.679274367739d+01")
   (with-data (vec vector-double 3)
     (setf (data vec) #(-3.21d0 1.0d0 12.8d0))
     (let ((mean (mean vec)))
       (lisp-unit:fp-sequence
	(list
	 mean
	 (variance vec)
	 (variance-m vec mean)
	 (standard-deviation vec)
	 (standard-deviation-m vec mean)
	 (variance-with-fixed-mean vec 4.0d0)
	 (standard-deviation-with-fixed-mean vec 4.0d0)))))
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
	  (variance-m vec mean)
	  (standard-deviation vec)
	  (standard-deviation-m vec mean)
	  (variance-with-fixed-mean vec 4.0d0)
	  (standard-deviation-with-fixed-mean vec 4.0d0))))))))
