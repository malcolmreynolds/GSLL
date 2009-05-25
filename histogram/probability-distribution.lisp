;; Histogram probability distribution.
;; Liam Healy, Mon Jan  1 2007 - 17:51
;; Time-stamp: <2009-05-24 16:28:15EDT probability-distribution.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_histogram.h
;;; /usr/include/gsl/gsl_histogram2d.h

(defmobject histogram-pdf
    "gsl_histogram_pdf"
  ((number-of-bins sizet))
  "one-dimensional histogram PDF"
  :documentation
  "Optionally initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   input-domain error is signalled because a probability distribution
   cannot contain negative values."
  :initialize-suffix "init"
  :initialize-args (((mpointer histogram) :pointer)))

(defmobject histogram2d-pdf
    "gsl_histogram2d_pdf"
  ((number-of-bins-x sizet) (number-of-bins-y sizet))
  "two-dimensional histogram PDF"
  :documentation
  "Optionally initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   input-domain error is signalled because a probability distribution
   cannot contain negative values."
  :initialize-suffix "init"
  :initialize-args (((mpointer histogram) :pointer)))

(defmfun sample ((value number) (pdf histogram-pdf) &key)
  "gsl_histogram_pdf_sample"
  (((mpointer pdf) :pointer) (value :double))
  :definition :method
  :c-return :double
  :documentation ;; FDL
  "Given a uniform random number (source) between zero and one,
   compute a single random sample from the probability distribution
   'pdf.  The algorithm used to compute the sample s is given by
   s = range[i] + delta * (range[i+1] - range[i])
   where i is the index which satisfies 
   sum[i] <=  value < sum[i+1] and delta is 
   (value - sum[i])/(sum[i+1] - sum[i]).")

(defmfun sample ((value number) (pdf histogram2d-pdf) &key)
  "gsl_histogram2d_pdf_sample"
  (((mpointer pdf) :pointer) (value :double))
  :definition :method
  :c-return :double)

(defmethod sample ((source random-number-generator) (pdf histogram-pdf) &key)
  (sample (sample source 'uniform) pdf))

(defmethod sample ((source random-number-generator) (pdf histogram2d-pdf) &key)
  (sample (sample source 'uniform) pdf))
