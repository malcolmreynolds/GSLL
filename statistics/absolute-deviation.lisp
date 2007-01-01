;********************************************************
; file:        absolute-deviation.lisp                   
; description: Absolute deviation                        
; date:        Sun Dec 31 2006 - 13:19                   
; author:      Liam M. Healy                             
; modified:    Sun Dec 31 2006 - 21:47
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defun-gsl absolute-deviation-nom (data)
  "gsl_stats_absdev"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) :size))
  :c-return :double
  :index absolute-deviation
  :export nil
  :documentation
  "The absolute deviation from the mean of @var{data}.  The
  absolute deviation from the mean is defined as
  absdev  = (1/N) \sum |x_i - \Hat\mu|
  where @math{x_i} are the elements of the dataset @var{data}.  The
  absolute deviation from the mean provides a more robust measure of the
  width of a distribution than the variance.  This function computes the
  mean of @var{data} via a call to #'mean.")

(defun-gsl absolute-deviation-m (data mean)
  "gsl_stats_absdev_m"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) :size) (mean :double))
  :c-return :double
  :index absolute-deviation
  :export nil
  :documentation
  "The absolute deviation of the dataset @var{data}
   relative to the given value of @var{mean},
   absdev  = (1/N) \sum |x_i - mean|.
   This function is useful if you have already computed the mean of
   @var{data} (and want to avoid recomputing it), or wish to calculate the
   absolute deviation relative to another value (such as zero, or the
   median).")

(export 'absolute-deviation)
(defun-optionals absolute-deviation (data &optional mean)
  -nom -m
  "The absolute deviation from the mean of @var{data}.  The
  absolute deviation from the mean is defined as
  absdev  = (1/N) \sum |x_i - \Hat\mu|
  where @math{x_i} are the elements of the dataset @var{data}.  The
  absolute deviation from the mean provides a more robust measure of the
  width of a distribution than the variance.  This function computes the
  mean of @var{data} via a call to #'mean.")

(defun-gsl weighted-absolute-deviation-nom (data weights)
  "gsl_stats_wabsdev"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array data) :pointer) (1 :int) ((dim0 data) :size))
  :c-return :double
  :index weighted-absolute-deviation
  :export nil)

(defun-gsl weighted-absolute-deviation-m (data weights mean)
  "gsl_stats_wabsdev_m"
  (((gsl-array weights) :pointer) (1 :int)
   ((gsl-array data) :pointer) (1 :int)
   ((dim0 data) :size) (mean :double))
  :c-return :double
  :index weighted-absolute-deviation
  :export nil)

(export 'weighted-absolute-deviation)
(defun-optionals weighted-absolute-deviation (data weights &optional mean)
  -nom -m
  "The weighted absolute deviation from the weighted
   mean, defined as
   absdev = (\sum w_i |x_i - \Hat\mu|) / (\sum w_i).")

(lisp-unit:define-test absolute-deviation
  (lisp-unit:assert-equal
   '("0.618000000000d+01" "0.664777777778d+01" "0.618000000000d+01")
   (with-data (vec vector-double 3)
     (setf (data vec) #(-3.21d0 1.0d0 12.8d0))
     (with-data (weights vector-double 3)
       (setf (data weights) #(3.0d0 1.0d0 2.0d0))
       (let ((mean (mean vec)))
	 (lisp-unit:fp-sequence
	  (list
	   (absolute-deviation vec)
	   (weighted-absolute-deviation vec weights)
	   (absolute-deviation vec mean))))))))
