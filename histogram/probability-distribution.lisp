;; Histogram probability distribution.
;; Liam Healy, Mon Jan  1 2007 - 17:51
;; Time-stamp: <2008-12-25 13:06:55EST probability-distribution.lisp>
;; $Id$

(in-package :gsl)

;; /usr/include/gsl/gsl_histogram.h
;; /usr/include/gsl/gsl_histogram2d.h

(defmobject histogram-pdf
    "gsl_histogram_pdf"
  ((number-of-bins sizet))
  "one-dimensional histogram PDF"
  "Optionally initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   input-domain error is signalled because a probability distribution
   cannot contain negative values."
  "init"
  (((mpointer histogram) :pointer)))

(defmobject histogram2d-pdf
    "gsl_histogram2d_pdf"
  ((number-of-bins-x sizet) (number-of-bins-y sizet))
  "two-dimensional histogram PDF"
  "Optionally initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   input-domain error is signalled because a probability distribution
   cannot contain negative values."
  "init"
  (((mpointer histogram) :pointer)))

(export 'sample)
(defgeneric sample (pdf value)
  (:documentation ;; FDL
   "Given a uniform random number between zero and one,
   compute a single random sample from the probability distribution
   'pdf.  The algorithm used to compute the sample s is given by
   s = range[i] + delta * (range[i+1] - range[i])
   where i is the index which satisfies 
   sum[i] <=  r < sum[i+1] and delta is 
   (r - sum[i])/(sum[i+1] - sum[i])."))

(defmfun sample ((pdf histogram-pdf) value)
  "gsl_histogram_pdf_sample"
  (((mpointer pdf) :pointer) (value :double))
  :definition :method
  :c-return :double)

(defmfun sample-2 (pdf value)
  "gsl_histogram2d_pdf_sample"
  (((mpointer pdf) :pointer) (value :double))
  :definition :method
  :c-return :double)
