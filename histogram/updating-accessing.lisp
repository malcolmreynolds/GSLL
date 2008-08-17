;; Updating and accessing histogram elements.
;; Liam Healy, Mon Jan  1 2007 - 14:43
;; Time-stamp: <2008-08-17 09:41:04EDT updating-accessing.lisp>
;; $Id$

(in-package :gsl)

(defmfun increment-fix (histogram value &optional weight)
  ("gsl_histogram_increment" "gsl_histogram_accumulate")
  ((((mpointer histogram) :pointer) (value :double))
   (((mpointer histogram) :pointer) (value :double) (weight :double)))
  :documentation
  "Update the histogram by adding the weight
   (which defaults to 1.0) to the
   bin whose range contains the coordinate x. 

   If x lies in the valid range of the histogram then the function
   returns zero to indicate success.  If x is less than the lower
   limit of the histogram then the function issues a warning :EDOM, and
   none of bins are modified.  Similarly, if the value of x is greater
   than or equal to the upper limit of the histogram then the function
   issues a warning :EDOM, and none of the bins are modified.  The error
   handler is not called, however, since it is often necessary to compute
   histograms for a small range of a larger dataset, ignoring the values
   outside the range of interest.")

(defmfun maref ((histogram histogram) &rest i)
  "gsl_histogram_get"
  (((pointer histogram) :pointer) ((first i) sizet))
  :definition :method 
  :c-return :double
  :documentation			; FDL
  "Return the contents of the i-th bin of the histogram.
   If i lies outside the valid range of indices for the
   histogram then an error (:EDOM) is signalled.")

(defmfun range (histogram i)
  "gsl_histogram_get_range"
  (((pointer histogram) :pointer) (i sizet)
   (lower :double) (upper :double))
  :documentation			; FDL
  "Find the upper and lower range limits of the i-th
   bin of the histogram.  If the index i is valid then the
   corresponding range limits are stored in lower and upper.
   The lower limit is inclusive (i.e. events with this coordinate are
   included in the bin) and the upper limit is exclusive (i.e. events with
   the coordinate of the upper limit are excluded and fall in the
   neighboring higher bin, if it exists).
   If i lies outside the valid range of indices for
   the histogram, then the error :EDOM is signalled.")

(defmfun max-range (histogram)
  "gsl_histogram_max"
  (((pointer histogram) :pointer))
  :c-return :double
  :documentation			; FDL
  "The maximum upper range limit of the histogram.")

(defmfun min-range (histogram)
  "gsl_histogram_min"
  (((pointer histogram) :pointer))
  :c-return :double
  :documentation			; FDL
  "The minimum lower range limit of the histogram.")

(defmfun bins (histogram)
  "gsl_histogram_bins"
  (((pointer histogram) :pointer))
  :c-return :int
  :documentation			; FDL
  "The number of bins in the histogram.")

(defmfun reset (histogram)
  "gsl_histogram_reset"
  (((pointer histogram) :pointer))
  :c-return :void
  :documentation			; FDL
  "Reset all the bins in the histogram to zero.")

(defmfun histogram-find (histogram x-value &optional y-value)
  ("gsl_histogram_find" "gsl_histogram2d_find")
  ((((mpointer histogram) :pointer) (x-value :double) (bin sizet))
   (((mpointer histogram) :pointer) (x-value :double) (y-value :double)
   (xbin sizet) (ybin sizet)))
  :documentation			; FDL
  "Finds the bin number which covers the coordinate value in
   the histogram.  The bin is located using a binary search. The
   search includes an optimization for histograms with uniform
   range, and will return the correct bin immediately in this
   case.  If the value is found in the range of the histogram
   then the function returns the index.  If value lies outside
   the valid range of the histogram then the error :EDOM is
   signalled.")

#|
 (letm ((histo (histogram 10)))		; should be a gsl-warning here, how to check?
     (set-ranges-uniform histo 0.0d0 10.0d0)
     (increment histo -2.0d0))
|#
;;; Examples and unit test

#|
(make-tests histogram
   ;; The first one gives a warning while compiling in SBCL,
   ;; should only give a warning while runnin.
 (letm ((histo (histogram 10)))
     (set-ranges-uniform histo 0.0d0 10.0d0)
     (increment histo -2.0d0))
 (letm ((histo (histogram 10)))
   (set-ranges-uniform histo 0.0d0 10.0d0)
   (increment histo 2.7d0)
   (increment histo 6.9d0 2.0d0)
   (maref histo 1))
 (letm ((histo (histogram 10)))
   (set-ranges-uniform histo 0.0d0 10.0d0)
   (increment histo 2.7d0)
   (increment histo 6.9d0 2.0d0)
   (maref histo 2))
 (letm ((histo (histogram 10)))
   (set-ranges-uniform histo 0.0d0 10.0d0)
   (increment histo 2.7d0)
   (increment histo 6.9d0 2.0d0)
   (maref histo 6))
 (letm ((histo (histogram 10)))
   (set-ranges-uniform histo 0.0d0 10.0d0)
   (increment histo 2.7d0)
   (increment histo 6.9d0 2.0d0)
   (maref histo 16))
 (letm ((histo (histogram 10)))
   (set-ranges-uniform histo 0.0d0 10.0d0)
   (increment histo 2.7d0)
   (increment histo 6.9d0 2.0d0)
   (values (gsl-min-range histo) (gsl-max-range histo)))
 (letm ((histo (histogram 10)))
   (set-ranges-uniform histo 0.0d0 10.0d0)
   (increment histo 2.7d0)
   (increment histo 6.9d0 2.0d0)
   (bins histo))
 (letm ((histo (histogram 10)))
   (set-ranges-uniform histo 0.0d0 10.0d0)
   (increment histo 2.7d0)
   (increment histo 6.9d0 2.0d0)
   (histogram-find histo 5.5d0)))
|#

(LISP-UNIT:DEFINE-TEST HISTOGRAM
  (LISP-UNIT:ASSERT-ERROR
   'GSL-CONDITION
   (LETM ((HISTO (HISTOGRAM 10)))
     (SET-RANGES-UNIFORM HISTO 0.0d0 10.0d0)
     (INCREMENT HISTO -2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((HISTO (HISTOGRAM 10)))
      (SET-RANGES-UNIFORM HISTO 0.0d0 10.0d0)
      (INCREMENT HISTO 2.7d0)
      (INCREMENT HISTO 6.9d0 2.0d0)
      (MAREF HISTO 1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((HISTO (HISTOGRAM 10)))
      (SET-RANGES-UNIFORM HISTO 0.0d0 10.0d0)
      (INCREMENT HISTO 2.7d0)
      (INCREMENT HISTO 6.9d0 2.0d0)
      (MAREF HISTO 2))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.0d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((HISTO (HISTOGRAM 10)))
      (SET-RANGES-UNIFORM HISTO 0.0d0 10.0d0)
      (INCREMENT HISTO 2.7d0)
      (INCREMENT HISTO 6.9d0 2.0d0)
      (MAREF HISTO 6))))
  (LISP-UNIT:ASSERT-ERROR
   'GSL-CONDITION
   (LETM ((HISTO (HISTOGRAM 10)))
     (SET-RANGES-UNIFORM HISTO 0.0d0 10.0d0)
     (INCREMENT HISTO 2.7d0)
     (INCREMENT HISTO 6.9d0 2.0d0)
     (MAREF HISTO 16)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0d0 10.0d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((HISTO (HISTOGRAM 10)))
      (SET-RANGES-UNIFORM HISTO 0.0d0 10.0d0)
      (INCREMENT HISTO 2.7d0)
      (INCREMENT HISTO 6.9d0 2.0d0)
      (VALUES (GSL-MIN-RANGE HISTO)
	      (GSL-MAX-RANGE HISTO)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 10)
   (MULTIPLE-VALUE-LIST
    (LETM ((HISTO (HISTOGRAM 10)))
      (SET-RANGES-UNIFORM HISTO 0.0d0 10.0d0)
      (INCREMENT HISTO 2.7d0)
      (INCREMENT HISTO 6.9d0 2.0d0)
      (BINS HISTO))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 5)
   (MULTIPLE-VALUE-LIST
    (LETM ((HISTO (HISTOGRAM 10)))
      (SET-RANGES-UNIFORM HISTO 0.0d0 10.0d0)
      (INCREMENT HISTO 2.7d0)
      (INCREMENT HISTO 6.9d0 2.0d0)
      (HISTOGRAM-FIND HISTO 5.5d0)))))
