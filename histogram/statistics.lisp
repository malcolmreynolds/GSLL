;********************************************************
; file:        statistics.lisp                           
; description: Statistics of histograms.                 
; date:        Mon Jan  1 2007 - 16:13                   
; author:      Liam M. Healy                             
; modified:    Mon Jan  1 2007 - 22:39
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl gsl-max-1 (histogram)
  "gsl_histogram_max_val"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index gsl-max
  :documentation
  "The maximum value contained in the histogram bins.")

(defun-gsl gsl-max-2 (histogram)
  "gsl_histogram2d_max_val"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index gsl-max
  :documentation
  "The maximum value contained in the histogram bins.")

(defmethod gsl-max ((histogram histogram))
  "The maximum value contained in the histogram bins."
  (histo-1d2d histogram gsl-max))

(defun-gsl gsl-min-1 (histogram)
  "gsl_histogram_min_val"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index gsl-min
  :documentation
  "The minimum value contained in the histogram bins.")

(defun-gsl gsl-min-2 (histogram)
  "gsl_histogram2d_min_val"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index gsl-min
  :documentation
  "The minimum value contained in the histogram bins.")

(defmethod gsl-min ((histogram histogram))
  "The minimum value contained in the histogram bins."
  (histo-1d2d histogram gsl-min))

(defun-gsl gsl-max-index-1 (histogram)
  "gsl_histogram_max_bin"
  (((pointer histogram) :pointer))
  :c-return :size
  :export nil
  :index gsl-max-index
  :documentation
  "The index of the bin containing the maximum value. In the case
   where several bins contain the same maximum value the smallest
   index is returned.")

(defun-gsl gsl-max-index-2 (histogram)
  "gsl_histogram2d_max_bin"
  (((pointer histogram) :pointer)
   (xindex :size) (yindex :size))
  :c-return :size
  :export nil
  :index gsl-max-index
  :documentation
  "The indices of the bin containing the maximum value. In the case
   where several bins contain the same maximum value the first bin
   found is returned.")

(defmethod gsl-max-index (histogram)
  (histo-1d2d histogram gsl-max-index))

(defun-gsl gsl-min-index-1 (histogram)
  "gsl_histogram_min_bin"
  (((pointer histogram) :pointer))
  :c-return :size
  :export nil
  :index gsl-min-index
  :documentation
  "The index of the bin containing the minimum value. In the case
   where several bins contain the same minimum value the smallest
   index is returned.")

(defun-gsl gsl-min-index-2 (histogram)
  "gsl_histogram2d_min_bin"
  (((pointer histogram) :pointer)
   (xindex :size) (yindex :size))
  :c-return :size
  :export nil
  :index gsl-min-index
  :documentation
  "The indices of the bin containing the minimum value. In the case
   where several bins contain the same minimum value the first bin
   found is returned.")

(defmethod gsl-min-index (histogram)
  (histo-1d2d histogram gsl-min-index))

(defun-gsl mean-1 (histogram)
  "gsl_histogram_mean"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index mean
  :documentation
  "The mean of the histogrammed variable, where the histogram is
   regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.  The
   resolution of the result is limited by the bin width.")

(defun-gsl mean-2x (histogram)
  "gsl_histogram2d_xmean"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index mean
  :documentation
  "The mean of the histogrammed x variable, where the histogram
   is regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.")

(defun-gsl mean-2y (histogram)
  "gsl_histogram2d_ymean"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index mean
  :documentation
  "The mean of the histogrammed y variable, where the histogram
   is regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.")

(defmethod mean ((histogram histogram))
  "The mean of the histogrammed y variable, where the histogram
   is regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.  For 2d
   histograms, the means are returned as multiple values."
  (flet ((mean-2 (histogram)
	   (values (mean-2x histogram) (mean-2y histogram))))
    (histo-1d2d histogram mean )))

(defun-gsl sigma-1 (histogram)
  "gsl_histogram_sigma"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sigma
  :documentation
  "The standard deviation of the histogrammed variable, where the
   histogram is regarded as a probability distribution. Negative
   bin values are ignored for the purposes of this
   calculation. The resolution of the result is limited by the bin
   width.")

(defun-gsl sigma-2x (histogram)
  "gsl_histogram2d_xsigma"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sigma)

(defun-gsl sigma-2y (histogram)
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
   multiple values."
  (flet ((sigma-2 (histogram)
	   (values (sigma-2x histogram) (sigma-2y histogram))))
    (histo-1d2d histogram sigma)))

(defun-gsl histogram-covariance (histogram-2d)
  "gsl_histogram2d_cov"
  (((pointer histogram-2d) :pointer))
  :c-return :double
  :documentation
  "The covariance of the histogrammed x and y variables, where
   the histogram is regarded as a probability
   distribution. Negative bin values are ignored for the purposes
   of this calculation.")

(defun-gsl sum-1 (histogram)
  "gsl_histogram_sum"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sum
  :documentation
  "The sum of all bin values. Negative bin values are included in
   the sum.")

(defun-gsl sum-2 (histogram)
  "gsl_histogram2d_sum"
  (((pointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sum
  :documentation
  "The sum of all bin values. Negative bin values are included in
   the sum.")

(export 'sum)
(defun sum (histogram)
  "The sum of all bin values. Negative bin values are included in
   the sum."
  (histo-1d2d histogram sum))
