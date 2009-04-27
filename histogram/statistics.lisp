;; Statistics of histograms.
;; Liam Healy, Mon Jan  1 2007 - 16:13
;; Time-stamp: <2009-04-26 23:15:55EDT statistics.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_histogram.h
;;; /usr/include/gsl/gsl_histogram2d.h

(defmfun mmax ((histogram histogram))
  "gsl_histogram_max_val"
  (((mpointer histogram) :pointer))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "The maximum value contained in the histogram bins.")

(defmfun mmax ((histogram histogram2d))
  "gsl_histogram2d_max_val"
  (((mpointer histogram) :pointer))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "The maximum value contained in the histogram bins.")

(defmfun mmin ((histogram histogram))
  "gsl_histogram_min_val"
  (((mpointer histogram) :pointer))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "The minimum value contained in the histogram bins.")

(defmfun mmin ((histogram histogram2d))
  "gsl_histogram2d_min_val"
  (((mpointer histogram) :pointer))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "The minimum value contained in the histogram bins.")

(defmfun max-index ((histogram histogram))
  "gsl_histogram_max_bin"
  (((mpointer histogram) :pointer))
  :definition :method
  :c-return sizet
  :documentation			; FDL
  "The index of the bin containing the maximum value. In the case
   where several bins contain the same maximum value the smallest
   index is returned.")

(defmfun max-index ((histogram histogram2d))
  "gsl_histogram2d_max_bin"
  (((mpointer histogram) :pointer)
   (xindex (:pointer sizet)) (yindex (:pointer sizet)))
  :definition :method
  :c-return :void
  :documentation			; FDL
  "The indices of the bin containing the maximum value. In the case
   where several bins contain the same maximum value the first bin
   found is returned.")

(defmfun min-index ((histogram histogram))
  "gsl_histogram_min_bin"
  (((mpointer histogram) :pointer))
  :definition :method
  :c-return sizet
  :documentation			; FDL
  "The index of the bin containing the minimum value. In the case
   where several bins contain the same minimum value the smallest
   index is returned.")

(defmfun min-index ((histogram histogram2d))
  "gsl_histogram2d_min_bin"
  (((mpointer histogram) :pointer)
   (xindex (:pointer sizet)) (yindex (:pointer sizet)))
  :definition :method
  :c-return :void
  :documentation			; FDL
  "The indices of the bin containing the minimum value. In the case
   where several bins contain the same minimum value the first bin
   found is returned.")

(defmfun mean ((histogram histogram))
  "gsl_histogram_mean"
  (((mpointer histogram) :pointer))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "The mean of the histogrammed variable, where the histogram is
   regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.  The
   resolution of the result is limited by the bin width.")

(defmethod mean ((histogram histogram2d))
  (values (mean-2x histogram) (mean-2y histogram)))

(defmfun mean-2x (histogram)
  "gsl_histogram2d_xmean"
  (((mpointer histogram) :pointer))
  :c-return :double
  :export nil
  :index mean
  :documentation			; FDL
  "The mean of the histogrammed x variable, where the histogram
   is regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.")

(defmfun mean-2y (histogram)
  "gsl_histogram2d_ymean"
  (((mpointer histogram) :pointer))
  :c-return :double
  :export nil
  :index mean
  :documentation			; FDL
  "The mean of the histogrammed y variable, where the histogram
   is regarded as a probability distribution. Negative bin values
   are ignored for the purposes of this calculation.")

(export 'sigma)
(defgeneric sigma (histogram)
  (:documentation ;; FDL
   "The standard deviation of the histogrammed variable, where the
   histogram is regarded as a probability distribution. Negative
   bin values are ignored for the purposes of this
   calculation. The resolution of the result is limited by the
   bin width.  For 2d histograms, the sigmas are returned as
   multiple values."))

(defmfun sigma ((histogram histogram))
  "gsl_histogram_sigma"
  (((mpointer histogram) :pointer))
  :c-return :double
  :definition :method)

(defmfun sigma-2x (histogram)
  "gsl_histogram2d_xsigma"
  (((mpointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sigma)

(defmfun sigma-2y (histogram)
  "gsl_histogram2d_ysigma"
  (((mpointer histogram) :pointer))
  :c-return :double
  :export nil
  :index sigma)

(defmethod sigma ((histogram histogram2d))
  (values (sigma-2x histogram) (sigma-2y histogram)))

(defmfun histogram-covariance (histogram-2d)
  "gsl_histogram2d_cov"
  (((mpointer histogram-2d) :pointer))
  :c-return :double
  :documentation			; FDL
  "The covariance of the histogrammed x and y variables, where
   the histogram is regarded as a probability
   distribution. Negative bin values are ignored for the purposes
   of this calculation.")

(export 'sum)
(defgeneric sum (histogram)
  (:documentation ;; FDL
   "The sum of all bin values. Negative bin values are included in
   the sum."))

(defmfun sum ((histogram histogram))
  "gsl_histogram_sum"
  (((mpointer histogram) :pointer))
  :c-return :double
  :definition :method)

(defmfun sum ((histogram histogram2d))
  "gsl_histogram2d_sum"
  (((mpointer histogram) :pointer))
  :definition :method
  :c-return :double)
