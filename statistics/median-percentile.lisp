;; Median and percentile
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-02-03 23:22:42EST median-percentile.lisp>
;; $Id: $

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defun-gsl median (sorted-data)
  "gsl_stats_median_from_sorted_data"
  (((gsl-array sorted-data) :pointer) (1 :int) ((dim0 sorted-data) :size))
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

(defun-gsl quantile (sorted-data fraction)
  "gsl_stats_quantile_from_sorted_data"
  (((gsl-array sorted-data) :pointer) (1 :int) ((dim0 sorted-data) :size)
   (fraction :double))
  :c-return :double
  :documentation			; FDL
  "A quantile value of sorted-data, gsl-vector-double.  The
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

(lisp-unit:define-test median-percentile
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (letm ((vec (vector-double #(-3.21d0 1.0d0 12.8d0))))
     (median vec)))
  (lisp-unit:assert-first-fp-equal
   "0.185000000000d+01"
   (letm ((vec (vector-double #(-18.0d0 -12.0d0 -3.21d0 0.5d0 1.0d0 2.7d0 12.8d0))))
     (quantile vec 0.75d0))))
