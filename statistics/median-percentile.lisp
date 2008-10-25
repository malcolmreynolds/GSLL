;; Median and percentile
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-10-25 18:50:07EDT median-percentile.lisp>
;; $Id$

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmfun median ((sorted-data vector))
  ("gsl_stats" :type "_median_from_sorted_data")
  (((c-pointer sorted-data) :pointer) (1 :int) ((dim0 sorted-data) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "The median value of sorted-data.  The elements of the array
   must be in ascending numerical order.  There are no checks to see
   whether the data are sorted, so the function #'sort should
   always be used first.
   When the dataset has an odd number of elements the median is the value
   of element (n-1)/2.  When the dataset has an even number of
   elements the median is the mean of the two nearest middle values,
   elements (n-1)/2 and n/2.  Since the algorithm for
   computing the median involves interpolation this function always returns
   a floating-point number, even for integer data types.")

(defmfun quantile ((sorted-data vector) fraction)
  ("gsl_stats" :type "_quantile_from_sorted_data")
  (((c-pointer sorted-data) :pointer) (1 :int) ((dim0 sorted-data) sizet)
   (fraction :double))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "A quantile value of sorted-data.  The
   elements of the array must be in ascending numerical order.  The
   quantile is determined by a fraction between 0 and 1.  For
   example, to compute the value of the 75th percentile
   'fraction should have the value 0.75.
   There are no checks to see whether the data are sorted, so the function
   #'sort should always be used first.
   \hbox{quantile} = (1 - \delta) x_i + \delta x_{i+1}
   where i is floor((n - 1)f) and \delta is (n-1)f - i.
   Thus the minimum value of the array (data[0*stride]) is given by
   'fraction equal to zero, the maximum value (data[(n-1)*stride]) is
   given by 'fraction equal to one and the median value is given by 'fraction
   equal to 0.5.  Since the algorithm for computing quantiles involves
   interpolation this function always returns a floating-point number, even
   for integer data types.")

;;; Examples and unit test

(save-test median-percentile
  (letm ((vec (vector-double-float (a -3.21d0 1.0d0 12.8d0))))
     (median vec))
  (letm ((vec (vector-double-float
	       (a -18.0d0 -12.0d0 -3.21d0 0.5d0 1.0d0 2.7d0 12.8d0))))
     (quantile vec 0.75d0)))
