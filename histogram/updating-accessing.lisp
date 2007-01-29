;********************************************************
; file:        updating-accessing.lisp                   
; description: Updating and accessing histogram          
;              elements.                                 
; date:        Mon Jan  1 2007 - 14:43                   
; author:      Liam M. Healy                             
; modified:    Sun Jan 28 2007 - 22:19
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl increment-fix (histogram value)
  "gsl_histogram_increment"
  (((pointer histogram) :pointer) (value :double))
  :index increment
  :export nil)

(defun-gsl increment-weight (histogram value weight)
  "gsl_histogram_accumulate"
  (((pointer histogram) :pointer) (value :double) (weight :double))
  :index increment
  :export nil)

(defun-optionals increment (histogram value &optional weight)
  -fix -weight
   "Update the histogram @var{h} by adding the weight
   (which defaults to 1.0) to the
   bin whose range contains the coordinate @var{x}. 

   If @var{x} lies in the valid range of the histogram then the function
   returns zero to indicate success.  If @var{x} is less than the lower
   limit of the histogram then the function issues a warning EDOM, and
   none of bins are modified.  Similarly, if the value of @var{x} is greater
   than or equal to the upper limit of the histogram then the function
   issues a warning EDOM, and none of the bins are modified.  The error
   handler is not called, however, since it is often necessary to compute
   histograms for a small range of a larger dataset, ignoring the values
   outside the range of interest.")

(defun-gsl gsl-aref ((histogram histogram) &rest i)
  "gsl_histogram_get"
  (((pointer histogram) :pointer) ((first i) :size))
  :type :method 
  :c-return :double
  :documentation
  "Return the contents of the @var{i}-th bin of the histogram.
   If @var{i} lies outside the valid range of indices for the
   histogram then an error (EDOM) is signalled.")

(defun-gsl range (histogram i)
  "gsl_histogram_get_range"
  (((pointer histogram) :pointer) (i :size)
   (lower :double) (upper :double))
  :documentation
  "Find the upper and lower range limits of the @var{i}-th
   bin of the histogram @var{h}.  If the index @var{i} is valid then the
   corresponding range limits are stored in @var{lower} and @var{upper}.
   The lower limit is inclusive (i.e. events with this coordinate are
   included in the bin) and the upper limit is exclusive (i.e. events with
   the coordinate of the upper limit are excluded and fall in the
   neighboring higher bin, if it exists).
   If @var{i} lies outside the valid range of indices for
   the histogram, then the error EDOM is signalled.")

(defun-gsl gsl-max-range (histogram)
  "gsl_histogram_max"
  (((pointer histogram) :pointer))
  :c-return :double
  :documentation
  "The maximum upper range limit of the histogram.")

(defun-gsl gsl-min-range (histogram)
  "gsl_histogram_min"
  (((pointer histogram) :pointer))
  :c-return :double
  :documentation
  "The minimum lower range limit of the histogram.")

(defun-gsl bins (histogram)
  "gsl_histogram_bins"
  (((pointer histogram) :pointer))
  :c-return :int
  :documentation
  "The number of bins in the histogram.")

(defun-gsl reset (histogram)
  "gsl_histogram_reset"
  (((pointer histogram) :pointer))
  :c-return :void
  :documentation
  "Reset all the bins in the histogram to zero.")

(defun-gsl histogram-find-1 (histogram value)
  "gsl_histogram_find"
  (((pointer histogram) :pointer) (value :double) (bin :size))
  :export nil
  :index histogram-find
  :documentation
  "Finds the bin number which covers the coordinate value in
   the histogram.  The bin is located using a binary search. The
   search includes an optimization for histograms with uniform
   range, and will return the correct bin immediately in this
   case.  If the value is found in the range of the histogram
   then the function returns the index.  If value lies outside
   the valid range of the histogram then the error EDOM is
   signalled.")

(defun-gsl histogram-find-2 (histogram x-value y-value)
  "gsl_histogram2d_find"
  (((pointer histogram) :pointer)
   (x-value :double) (y-value :double)
   (xbin :size) (ybin :size))
  :export nil
  :index histogram-find)

(export 'histogram-find)
(defun histogram-find (histogram &rest values)
  (histo-1d2d histogram histogram-find
	      ((first values))
	      ((first values) (second values))))

;;; Examples and unit test
(defparameter *sample-histogram*
  (let ((histo (make-histogram 10)))
    (set-ranges-uniform histo 0.0d0 10.0d0)
    histo))

(defun setup-sample-histogram ()
  (reset *sample-histogram*)
  (increment *sample-histogram* 2.7d0)
  (increment *sample-histogram* 6.9d0 2.0d0))

(lisp-unit:define-test histogram
  (lisp-unit:assert-error
   'gsl-warning
   (progn
     (setup-sample-histogram)
     (increment *sample-histogram* -2.0d0)))
  (lisp-unit:assert-first-fp-equal
   "0.000000000000d+01"
   (progn
     (setup-sample-histogram)
     (gsl-aref *sample-histogram* 1)))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (progn
     (setup-sample-histogram)
     (gsl-aref *sample-histogram* 2)))
  (lisp-unit:assert-first-fp-equal
   "0.200000000000d+01"
   (progn
     (setup-sample-histogram)
     (gsl-aref *sample-histogram* 6)))
  (lisp-unit:assert-error
   'gsl-error
   (gsl-aref *sample-histogram* 16))
  (lisp-unit:assert-first-fp-equal
   "0.000000000000d+01"
   (gsl-min-range *sample-histogram*))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+02"
   (gsl-max-range *sample-histogram*))
  (lisp-unit:assert-eql
   10
   (bins *sample-histogram*))
  (lisp-unit:assert-eql
   5
   (histogram-find *sample-histogram* 5.5d0)))

  
