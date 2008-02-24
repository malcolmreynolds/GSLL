;; Histogram operations
;; Liam Healy, Mon Jan  1 2007 - 16:47
;; Time-stamp: <2008-02-23 18:57:17EST operations.lisp>
;; $Id: $

(in-package :gsl)

(defmfun equal-bins-p-1 (histogram1 histogram2)
  "gsl_histogram_equal_bins_p"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :c-return :true-false
  :export nil
  :index equal-bins-p
  :documentation			; FDL
  "Are all of the individual bin ranges of the two histograms are identical?")

(defmfun equal-bins-p-2 (histogram1 histogram2)
  "gsl_histogram2d_equal_bins_p"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :c-return :true-false
  :export nil
  :index equal-bins-p
  :documentation			; FDL
  "Are all of the individual bin ranges of the two histograms are identical?")

(export 'equal-bins-p)
(defun equal-bins-p (histogram1 histogram2)
  "Are all of the individual bin ranges of the two histograms are identical?"
  (histo-1d2d histogram1 equal-bins-p (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m+-1 (histogram1 histogram2)
  "gsl_histogram_add"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index m+
  :documentation			; FDL
  "Add the contents of the bins in histogram2 to the
   corresponding bins of histogram1 i.e. h'_1(i) =
   h_1(i) + h_2(i).  The two histograms must have identical bin
   ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m+-2 (histogram1 histogram2)
  "gsl_histogram2d_add"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index m+
  :documentation			; FDL
  "Add the contents of the bins in histogram2 to the
   corresponding bins of histogram1 i.e. h'_1(i) =
   h_1(i) + h_2(i).  The two histograms must have identical bin
   ranges.")

(defmethod m+ ((histogram1 histogram) (histogram2 histogram))
  ;; FDL
  "Add the contents of the bins in histogram2 to the
   corresponding bins of histogram1 i.e. h'_1(i) =
   h_1(i) + h_2(i).  The two histograms must have identical bin
   ranges."
  (histo-1d2d histogram1 m+ (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m--1 (histogram1 histogram2)
  "gsl_histogram_sub"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index m-
  :documentation			; FDL
  "Subtract the contents of the bins in histogram2 from the
   corresponding bins of histogram1 i.e. h'_1(i) = h_1(i) -
   h_2(i).  The two histograms must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m--2 (histogram1 histogram2)
  "gsl_histogram2d_sub"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index m-
  :documentation			; FDL
  "Subtract the contents of the bins in histogram2 from the
   corresponding bins of histogram1 i.e. h'_1(i) = h_1(i) -
   h_2(i).  The two histograms must have identical bin ranges.")

(defmethod m- ((histogram1 histogram) (histogram2 histogram))
  ;; FDL
  "Subtract the contents of the bins in histogram2 from the
   corresponding bins of histogram1 i.e. h'_1(i) = h_1(i) -
   h_2(i).  The two histograms must have identical bin ranges."
  (histo-1d2d histogram1 gsl- (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m*-1 (histogram1 histogram2)
  "gsl_histogram_mul"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index m*
  :documentation			; FDL
  "Multiply the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) * h_2(i).  The two histograms
   must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m*-2 (histogram1 histogram2)
  "gsl_histogram2d_mul"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index m*
  :documentation			; FDL
  "Multiply the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) * h_2(i).  The two histograms
   must have identical bin ranges.")

(defmethod m* ((histogram1 histogram) (histogram2 histogram))
  ;; FDL
  "Multiply the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) * h_2(i).  The two histograms
   must have identical bin ranges."
  (histo-1d2d histogram1 m* (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m/-1 (histogram1 histogram2)
  "gsl_histogram_div"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index m/
  :documentation			; FDL
  "Divide the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) / h_2(i).  The two histograms
   must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun m/-2 (histogram1 histogram2)
  "gsl_histogram2d_div"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index m/
  :documentation			; FDL
  "Divide the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) / h_2(i).  The two histograms
   must have identical bin ranges.")

(defmethod m/ ((histogram1 histogram) (histogram2 histogram))
  ;; FDL
  "Divide the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. h'_1(i) = h_1(i) / h_2(i).  The two histograms
   must have identical bin ranges."
  (histo-1d2d histogram1 m/ (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun scale-1 (histogram scale)
  "gsl_histogram_scale"
  (((pointer histogram) :pointer) (scale :double))
  :export nil
  :index scale
  :documentation			; FDL
  "Multiply the contents of the bins of histogram by the
   constant scale, i.e. h'_1(i) = h_1(i) * scale.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun scale-2 (histogram scale)
  "gsl_histogram2d_scale"
  (((pointer histogram) :pointer) (scale :double))
  :export nil
  :index scale
  :documentation			; FDL
  "Multiply the contents of the bins of histogram by the
   constant scale, i.e. h'_1(i) = h_1(i) * scale.")

(export 'scale)
(defun scale (histogram scale)
  ;; FDL
  "Multiply the contents of the bins of histogram by the
   constant scale, i.e. h'_1(i) = h_1(i) * scale."
  (histo-1d2d histogram scale (scale)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun shift-1 (histogram offset)
  "gsl_histogram_shift"
  (((pointer histogram) :pointer) (offset :double))
  :export nil
  :index shift
  :documentation			; FDL
  "Shift the contents of the bins of histogram h by the
   constant offset, i.e. h'_1(i) = h_1(i) + offset.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defmfun shift-2 (histogram offset)
  "gsl_histogram2d_shift"
  (((pointer histogram) :pointer) (offset :double))
  :export nil
  :index shift
  :documentation			; FDL
  "Shift the contents of the bins of histogram h by the
   constant offset, i.e. h'_1(i) = h_1(i) + offset.")

(export 'shift)
(defun shift (histogram offset)
  (histo-1d2d histogram shift (offset)))
