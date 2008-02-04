;; Skewness and kurtosis.
;; Liam Healy, Sun Dec 31 2006 - 14:20
;; Time-stamp: <2008-02-03 23:14:05EST higher-moments.lisp>
;; $Id: $

(in-package :gsl)
;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defun-gsl skewness-nomsd (data)
  "gsl_stats_skew"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) :size))
  :c-return :double
  :index skewness
  :export nil
  :documentation			; FDL
  "The skewness of data, defined as
  skew = (1/N) \sum ((x_i - \Hat\mu)/\Hat\sigma)^3
  where x_i are the elements of the dataset data.
  The skewness measures the asymmetry of the tails of a distribution.")

(defun-gsl skewness-msd (data mean standard-deviation)
  "gsl_stats_skew_m_sd"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) :size) (mean :double) (standard-deviation :double))
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

(defun-gsl kurtosis-nomsd (data)
  "gsl_stats_kurtosis"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) :size))
  :c-return :double
  :index kurtosis
  :export nil)

(defun-gsl kurtosis-msd (data mean standard-deviation)
  "gsl_stats_kurtosis_m_sd"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) :size) (mean :double) (standard-deviation :double))
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

(defun-gsl weighted-skewness-nomsd (data weights)
  "gsl_stats_wskew"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array data) :pointer) (1 :int) ((dim0 data) :size))
  :c-return :double
  :index weighted-skewness
  :export nil)

(defun-gsl weighted-skewness-msd (data weights mean standard-deviation)
  "gsl_stats_wskew_m_sd"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) :size) (mean :double) (standard-deviation :double))
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

(defun-gsl weighted-kurtosis-nomsd (data weights)
  "gsl_stats_wkurtosis"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) :size))
  :c-return :double
  :index weighted-kurtosis
  :export nil)

(defun-gsl weighted-kurtosis-msd (data weights mean standard-deviation)
  "gsl_stats_wkurtosis_m_sd"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) :size) (mean :double) (standard-deviation :double))
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

(lisp-unit:define-test higher-moments
  (lisp-unit:assert-equal
   '("0.276511898399d+00" "0.276511898399d+00"
     "-0.233333333333d+01" "-0.233333333333d+01")
   (lisp-unit:fp-sequence
    (letm ((vec (vector-double #(-3.21d0 1.0d0 12.8d0))))
      (let* ((mean (mean vec))
	     (sd (standard-deviation vec mean)))
	(list
	 (skewness vec)
	 (skewness vec mean sd)
	 (kurtosis vec)
	 (kurtosis vec mean sd)))))))
