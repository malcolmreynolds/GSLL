;; Absolute deviation
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-02-17 16:39:38EST absolute-deviation.lisp>
;; $Id: $

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmfun absolute-deviation-nom (data)
  "gsl_stats_absdev"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) size))
  :c-return :double
  :index absolute-deviation
  :export nil
  :documentation			; FDL
  "The absolute deviation from the mean of data.  The
  absolute deviation from the mean is defined as
  absdev  = (1/N) \sum |x_i - \Hat\mu|
  where x_i are the elements of the dataset data.  The
  absolute deviation from the mean provides a more robust measure of the
  width of a distribution than the variance.  This function computes the
  mean of data via a call to #'mean.")

(defmfun absolute-deviation-m (data mean)
  "gsl_stats_absdev_m"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) size) (mean :double))
  :c-return :double
  :index absolute-deviation
  :export nil
  :documentation			; FDL
  "The absolute deviation of the dataset data
   relative to the given value of mean,
   absdev  = (1/N) \sum |x_i - mean|.
   This function is useful if you have already computed the mean of
   data (and want to avoid recomputing it), or wish to calculate the
   absolute deviation relative to another value (such as zero, or the
   median).")

(export 'absolute-deviation)
(defun-optionals absolute-deviation (data &optional mean)
  -nom -m
  ;; FDL
  "The absolute deviation from the mean of data.  The
  absolute deviation from the mean is defined as
  absdev  = (1/N) \sum |x_i - \Hat\mu|
  where x_i are the elements of the dataset data.  The
  absolute deviation from the mean provides a more robust measure of the
  width of a distribution than the variance.  This function computes the
  mean of data via a call to #'mean.")

(defmfun weighted-absolute-deviation-nom (data weights)
  "gsl_stats_wabsdev"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array data) :pointer) (1 :int) ((dim0 data) size))
  :c-return :double
  :index weighted-absolute-deviation
  :export nil)

(defmfun weighted-absolute-deviation-m (data weights mean)
  "gsl_stats_wabsdev_m"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array data) :pointer) (1 :int)
   ((dim0 data) size) (mean :double))
  :c-return :double
  :index weighted-absolute-deviation
  :export nil)

(export 'weighted-absolute-deviation)
(defun-optionals weighted-absolute-deviation (data weights &optional mean)
  -nom -m
  ;; FDL
  "The weighted absolute deviation from the weighted
   mean, defined as
   absdev = (\sum w_i |x_i - \Hat\mu|) / (\sum w_i).")

;;; Examples and unit test

#|
(make-tests absolute-deviation
  (letm ((vec (vector-double #(-3.21d0 1.0d0 12.8d0)))
	   (weights (vector-double #(3.0d0 1.0d0 2.0d0))))
      (let ((mean (mean vec)))
	(list
	 (absolute-deviation vec)
	 (weighted-absolute-deviation vec weights)
	 (absolute-deviation vec mean)))))
|#

(LISP-UNIT:DEFINE-TEST ABSOLUTE-DEVIATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 6.18d0 6.647777777777779d0 6.18d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE #(-3.21d0 1.0d0 12.8d0)))
	   (WEIGHTS (VECTOR-DOUBLE #(3.0d0 1.0d0 2.0d0))))
      (LET ((MEAN (MEAN VEC)))
	(LIST (ABSOLUTE-DEVIATION VEC)
	      (WEIGHTED-ABSOLUTE-DEVIATION VEC WEIGHTS)
	      (ABSOLUTE-DEVIATION VEC MEAN)))))))

