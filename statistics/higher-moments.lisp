;; Skewness and kurtosis.
;; Liam Healy, Sun Dec 31 2006 - 14:20
;; Time-stamp: <2008-02-17 16:42:59EST higher-moments.lisp>
;; $Id$

(in-package :gsl)
;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmfun skewness-nomsd (data)
  "gsl_stats_skew"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) size))
  :c-return :double
  :index skewness
  :export nil
  :documentation			; FDL
  "The skewness of data, defined as
  skew = (1/N) \sum ((x_i - \Hat\mu)/\Hat\sigma)^3
  where x_i are the elements of the dataset data.
  The skewness measures the asymmetry of the tails of a distribution.")

(defmfun skewness-msd (data mean standard-deviation)
  "gsl_stats_skew_m_sd"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) size) (mean :double) (standard-deviation :double))
  :c-return :double
  :index skewness
  :export nil
  :documentation			; FDL
  "The skewness of the dataset data using the
   given values of the mean and standard deviation,
   skew = (1/N) \sum ((x_i - mean)/sd)^3
   These functions are useful if you have already computed the mean and
   standard deviation of data and want to avoid recomputing them.")

(export 'skewness)
(defun-optionals skewness
    (data &optional mean standard-deviation)
  -nomsd -msd
  ;; FDL
  "The skewness of data defined as
  skew = (1/N) \sum ((x_i - \Hat\mu)/\Hat\sigma)^3
  where x_i are the elements of the dataset data.
  The skewness measures the asymmetry of the tails of a distribution.")

(defmfun kurtosis-nomsd (data)
  "gsl_stats_kurtosis"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) size))
  :c-return :double
  :index kurtosis
  :export nil)

(defmfun kurtosis-msd (data mean standard-deviation)
  "gsl_stats_kurtosis_m_sd"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) size) (mean :double) (standard-deviation :double))
  :c-return :double
  :index kurtosis
  :export nil)

(export 'kurtosis)
(defun-optionals kurtosis
    (data &optional mean standard-deviation)
  -nomsd -msd
  ;; FDL
  "The kurtosis of data defined as
   kurtosis = ((1/N) \sum ((x_i - \Hat\mu)/\Hat\sigma)^4)  - 3
   The kurtosis measures how sharply peaked a distribution is,
   relative to its width.  The kurtosis is normalized to zero
   for a gaussian distribution.")

(defmfun weighted-skewness-nomsd (data weights)
  "gsl_stats_wskew"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array data) :pointer) (1 :int) ((dim0 data) size))
  :c-return :double
  :index weighted-skewness
  :export nil)

(defmfun weighted-skewness-msd (data weights mean standard-deviation)
  "gsl_stats_wskew_m_sd"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) size) (mean :double) (standard-deviation :double))
  :c-return :double
  :index weighted-skewness
  :export nil)

(export 'weighted-skewness)
(defun-optionals weighted-skewness
    (data weights &optional mean standard-deviation)
  -nomsd -msd
  ;; FDL
  "The weighted skewness of the dataset.
   skew = (\sum w_i ((x_i - xbar)/\sigma)^3) / (\sum w_i).")

(defmfun weighted-kurtosis-nomsd (data weights)
  "gsl_stats_wkurtosis"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) size))
  :c-return :double
  :index weighted-kurtosis
  :export nil)

(defmfun weighted-kurtosis-msd (data weights mean standard-deviation)
  "gsl_stats_wkurtosis_m_sd"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) size) (mean :double) (standard-deviation :double))
  :c-return :double
  :index weighted-kurtosis
  :export nil)

(export 'weighted-kurtosis)
(defun-optionals weighted-kurtosis
    (data weights &optional mean standard-deviation)
  -nomsd -msd
  ;; FDL
  "The weighted kurtosis of the dataset.
   kurtosis = ((\sum w_i ((x_i - xbar)/sigma)^4) / (\sum w_i)) - 3.")

;;; Examples and unit test

#|
(make-tests higher-moments
  (letm ((vec (vector-double #(-3.21d0 1.0d0 12.8d0))))
      (let* ((mean (mean vec))
	     (sd (standard-deviation vec mean)))
	(list
	 (skewness vec)
	 (skewness vec mean sd)
	 (kurtosis vec)
	 (kurtosis vec mean sd)))))
|#

(LISP-UNIT:DEFINE-TEST HIGHER-MOMENTS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.2765118983985497d0 0.2765118983985497d0
	  -2.333333333333333d0 -2.333333333333333d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE #(-3.21d0 1.0d0 12.8d0))))
      (LET* ((MEAN (MEAN VEC))
	     (SD (STANDARD-DEVIATION VEC MEAN)))
	(LIST (SKEWNESS VEC) (SKEWNESS VEC MEAN SD)
	      (KURTOSIS VEC)
	      (KURTOSIS VEC MEAN SD)))))))
