;; Histogram operations
;; Liam Healy, Mon Jan  1 2007 - 16:47
;; Time-stamp: <2008-12-26 14:47:49EST operations.lisp>
;; $Id$

(in-package :gsl)

(export 'equal-bins-p)
(defgeneric equal-bins-p (histogram1 histogram2)
  (:documentation ;; FDL
   "Are all of the individual bin ranges of the two histograms are identical?"))

(defmfun equal-bins-p ((histogram1 histogram) (histogram2 histogram))
  "gsl_histogram_equal_bins_p"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :c-return :true-false)

(defmfun equal-bins-p ((histogram1 histogram2d) (histogram2 histogram2d))
  "gsl_histogram2d_equal_bins_p"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :c-return :true-false)

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m+ ((histogram1 histogram) (histogram2 histogram))
  "gsl_histogram_add"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :documentation			; FDL
  "Add the contents of the bins in histogram2 to the
   corresponding bins of histogram1 i.e. h'_1(i) =
   h_1(i) + h_2(i).  The two histograms must have identical bin
   ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m+ ((histogram1 histogram2d) (histogram2 histogram2d))
  "gsl_histogram2d_add"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :documentation			; FDL
  "Add the contents of the bins in histogram2 to the
   corresponding bins of histogram1 i.e. h'_1(i) =
   h_1(i) + h_2(i).  The two histograms must have identical bin
   ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m- ((histogram1 histogram) (histogram2 histogram))
  "gsl_histogram_sub"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :documentation			; FDL
  "Subtract the contents of the bins in histogram2 from the
   corresponding bins of histogram1 i.e. h'_1(i) = h_1(i) -
   h_2(i).  The two histograms must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m- ((histogram1 histogram2d) (histogram2 histogram2d))
  "gsl_histogram2d_sub"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :documentation			; FDL
  "Subtract the contents of the bins in histogram2 from the
   corresponding bins of histogram1 i.e. h'_1(i) = h_1(i) -
   h_2(i).  The two histograms must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m* ((histogram1 histogram) (histogram2 histogram))
  "gsl_histogram_mul"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :documentation			; FDL
  "Multiply the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) * h_2(i).  The two histograms
   must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m* ((histogram1 histogram2d) (histogram2 histogram2d))
  "gsl_histogram2d_mul"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :documentation			; FDL
  "Multiply the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) * h_2(i).  The two histograms
   must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m/ ((histogram1 histogram) (histogram2 histogram))
  "gsl_histogram_div"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :documentation			; FDL
  "Divide the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) / h_2(i).  The two histograms
   must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m/ ((histogram1 histogram2d) (histogram2 histogram2d))
  "gsl_histogram2d_div"
  (((mpointer histogram1) :pointer) ((mpointer histogram2) :pointer))
  :definition :method
  :documentation			; FDL
  "Divide the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) / h_2(i).  The two histograms
   must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun scale ((histogram histogram) scale)
  "gsl_histogram_scale"
  (((mpointer histogram) :pointer) (scale :double))
  :definition :method
  :documentation			; FDL
  "Multiply the contents of the bins of histogram by the
   constant scale, i.e. h'_1(i) = h_1(i) * scale.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun scale ((histogram histogram2d) scale)
  "gsl_histogram2d_scale"
  (((mpointer histogram) :pointer) (scale :double))
  :definition :method
  :documentation			; FDL
  "Multiply the contents of the bins of histogram by the
   constant scale, i.e. h'_1(i) = h_1(i) * scale.")

(export 'shift)
(defgeneric shift (histogram offset)
  (:documentation ;; FDL
   "Shift the contents of the bins of histogram h by the
   constant offset, i.e. h'_1(i) = h_1(i) + offset."))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun shift ((histogram histogram) offset)
  "gsl_histogram_shift"
  (((mpointer histogram) :pointer) (offset :double))
  :definition :method)

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun shift ((histogram histogram2d) offset)
  "gsl_histogram2d_shift"
  (((mpointer histogram) :pointer) (offset :double))
  :definition :method)
