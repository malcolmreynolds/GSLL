;; Statistics of histograms.
;; Liam Healy, Mon Jan  1 2007 - 16:13
;; Time-stamp: <2008-02-17 17:07:06EST statistics.lisp>
;; $Id: $

(in-package :gsl)

(defmfun gsl-max-1 (histogram)
  "gsl_histogram_max_val"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index gsl-max
  :documentation			; FDL
  "The maximum value contained in the histogram bins.")

(defmfun gsl-max-2 (histogram)
  "gsl_histogram2d_max_val"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index gsl-max
  :documentation			; FDL
  "The maximum value contained in the histogram bins.")

(defmethod gsl-max ((histogram histogram))
  "The maximum value contained in the histogram bins."
  (histo-1d2d histogram gsl-max))

(defmfun gsl-min-1 (histogram)
  "gsl_histogram_min_val"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index gsl-min
  :documentation			; FDL
  "The minimum value contained in the histogram bins.")

(defmfun gsl-min-2 (histogram)
  "gsl_histogram2d_min_val"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index gsl-min
  :documentation			; FDL
  "The minimum value contained in the histogram bins.")

(defmethod gsl-min ((histogram histogram)) ; FDL
  "The minimum value contained in the histogram bins."
  (histo-1d2d histogram gsl-min))

(defmfun gsl-max-index-1 (histogram)
  "gsl_histogram_max_bin"
  (((pointer histogram) :pointer))
  :c-return size
  :export nil
  :index gsl-max-index
  :documentation			; FDL
  "The index of the bin containing the maximum value. In the case
   where several bins contain the same maximum value the smallest
   index is returned.")

(defmfun gsl-max-index-2 (histogram)
  "gsl_histogram2d_max_bin"
  (((pointer histogram) :pointer)
   (xindex size) (yindex size))
  :c-return size
  :export nil
  :index gsl-max-index
  :documentation			; FDL
  "The indices of the bin containing the maximum value. In the case
   where several bins contain the same maximum value the first bin
   found is returned.")

(defmethod gsl-max-index (histogram)
  (histo-1d2d histogram gsl-max-index))

(defmfun gsl-min-index-1 (histogram)
  "gsl_histogram_min_bin"
  (((pointer histogram) :pointer))
  :c-return size
  :export nil
  :index gsl-min-index
  :documentation			; FDL
  "The index of the bin containing the minimum value. In the case
   where several bins contain the same minimum value the smallest
   index is returned.")

(defmfun gsl-min-index-2 (histogram)
  "gsl_histogram2d_min_bin"
  (((pointer histogram) :pointer)
   (xindex size) (yindex size))
  :c-return size
  :export nil
  :index gsl-min-index
  :documentation			; FDL
  "The indices of the bin containing the minimum value. In the case
   where several bins contain the same minimum value the first bin
   found is returned.")

(defmethod gsl-min-index (histogram)
  (histo-1d2d histogram gsl-min-index))

(defmfun mean-1 (histogram)
  "gsl_histogram_mean"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index mean
  :documentation			; FDL
  "The mean of the histogrammed variable, where the histogram is
   regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.  The
   resolution of the result is limited by the bin width.")

(defmfun mean-2x (histogram)
  "gsl_histogram2d_xmean"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index mean
  :documentation			; FDL
  "The mean of the histogrammed x variable, where the histogram
   is regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.")

(defmfun mean-2y (histogram)
  "gsl_histogram2d_ymean"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index mean
  :documentation			; FDL
  "The mean of the histogrammed y variable, where the histogram
   is regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.")

(defmethod mean ((histogram histogram))	; FDL
  "The mean of the histogrammed y variable, where the histogram
   is regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.  For 2d
   histograms, the means are returned as multiple values."
  (flet ((mean-2 (histogram)
	   (values (mean-2x histogram) (mean-2y histogram))))
    (histo-1d2d histogram mean )))

(defmfun sigma-1 (histogram)
  "gsl_histogram_sigma"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sigma
  :documentation			; FDL
  "The standard deviation of the histogrammed variable, where the
   histogram is regarded as a probability distribution. Negative
   bin values are ignored for the purposes of this
   calculation. The resolution of the result is limited by the bin
   width.")

(defmfun sigma-2x (histogram)
  "gsl_histogram2d_xsigma"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sigma)

(defmfun sigma-2y (histogram)
  "gsl_histogram2d_ysigma"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sigma)

(export 'sigma)
(defun sigma (histogram)
  "The standard deviation of the histogrammed variable, where the
   histogram is regarded as a probability distribution. Negative
   bin values are ignored for the purposes of this
   calculation. The resolution of the result is limited by the
   bin width.  For 2d histograms, the sigmas are returned as
   multiple values."			; FDL
  (flet ((sigma-2 (histogram)
	   (values (sigma-2x histogram) (sigma-2y histogram))))
    (histo-1d2d histogram sigma)))

(defmfun histogram-covariance (histogram-2d)
  "gsl_histogram2d_cov"
  (((pointer histogram-2d) :pointer))
  :c-return :double
  :documentation			; FDL
  "The covariance of the histogrammed x and y variables, where
   the histogram is regarded as a probability
   distribution. Negative bin values are ignored for the purposes
   of this calculation.")

(defmfun sum-1 (histogram)
  "gsl_histogram_sum"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sum
  :documentation			; FDL
  "The sum of all bin values. Negative bin values are included in
   the sum.")

(defmfun sum-2 (histogram)
  "gsl_histogram2d_sum"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sum
  :documentation			; FDL
  "The sum of all bin values. Negative bin values are included in
   the sum.")

(export 'sum)
(defun sum (histogram)
  ;; FDL
  "The sum of all bin values. Negative bin values are included in
   the sum."
  (histo-1d2d histogram sum))
