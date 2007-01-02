;********************************************************
; file:        operations.lisp                           
; description: Histogram operations                      
; date:        Mon Jan  1 2007 - 16:47                   
; author:      Liam M. Healy                             
; modified:    Mon Jan  1 2007 - 21:50
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl equal-bins-p-1 (histogram1 histogram2)
  "gsl_histogram_equal_bins_p"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :c-return :true-false
  :export nil
  :index equal-bins-p
  :documentation
  "Are all of the individual bin
   ranges of the two histograms are identical?")

(defun-gsl equal-bins-p-2 (histogram1 histogram2)
  "gsl_histogram2d_equal_bins_p"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :c-return :true-false
  :export nil
  :index equal-bins-p
  :documentation
  "Are all of the individual bin
   ranges of the two histograms are identical?")

(export 'equal-bins-p)
(defun equal-bins-p (histogram1 histogram2)
  "Are all of the individual bin
   ranges of the two histograms are identical?"
  (histo-1d2d histogram1 equal-bins-p (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl gsl+-1 (histogram1 histogram2)
  "gsl_histogram_add"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index gsl+
  :documentation
  "Add the contents of the bins in histogram2 to the
   corresponding bins of histogram1 i.e. @math{h'_1(i) =
   h_1(i) + h_2(i)}.  The two histograms must have identical bin
   ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl gsl+-2 (histogram1 histogram2)
  "gsl_histogram2d_add"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index gsl+
  :documentation
  "Add the contents of the bins in histogram2 to the
   corresponding bins of histogram1 i.e. @math{h'_1(i) =
   h_1(i) + h_2(i)}.  The two histograms must have identical bin
   ranges.")

(defmethod gsl+ ((histogram1 histogram) (histogram2 histogram))
  "Add the contents of the bins in histogram2 to the
   corresponding bins of histogram1 i.e. @math{h'_1(i) =
   h_1(i) + h_2(i)}.  The two histograms must have identical bin
   ranges."
  (histo-1d2d histogram1 gsl+ (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl gsl--1 (histogram1 histogram2)
  "gsl_histogram_sub"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index gsl-
  :documentation
  "Subtract the contents of the bins in histogram2 from the
   corresponding bins of histogram1 i.e. @math{h'_1(i) = h_1(i) -
   h_2(i)}.  The two histograms must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl gsl--2 (histogram1 histogram2)
  "gsl_histogram2d_sub"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index gsl-
  :documentation
  "Subtract the contents of the bins in histogram2 from the
   corresponding bins of histogram1 i.e. @math{h'_1(i) = h_1(i) -
   h_2(i)}.  The two histograms must have identical bin ranges.")

(defmethod gsl- ((histogram1 histogram) (histogram2 histogram))
  "Subtract the contents of the bins in histogram2 from the
   corresponding bins of histogram1 i.e. @math{h'_1(i) = h_1(i) -
   h_2(i)}.  The two histograms must have identical bin ranges."
  (histo-1d2d histogram1 gsl- (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl gsl*-1 (histogram1 histogram2)
  "gsl_histogram_mul"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index gsl*
  :documentation
  "Multiply the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. @math{h'_1(i) = h_1(i) * h_2(i)}.  The two histograms
   must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl gsl*-2 (histogram1 histogram2)
  "gsl_histogram2d_mul"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index gsl*
  :documentation
  "Multiply the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. @math{h'_1(i) = h_1(i) * h_2(i)}.  The two histograms
   must have identical bin ranges.")

(defmethod gsl* ((histogram1 histogram) (histogram2 histogram))
  "Multiply the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. @math{h'_1(i) = h_1(i) * h_2(i)}.  The two histograms
   must have identical bin ranges."
  (histo-1d2d histogram1 gsl* (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl gsl/-1 (histogram1 histogram2)
  "gsl_histogram_div"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index gsl/
  :documentation
  "Divide the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. @math{h'_1(i) = h_1(i) / h_2(i)}.  The two histograms
   must have identical bin ranges.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl gsl/-2 (histogram1 histogram2)
  "gsl_histogram2d_div"
  (((pointer histogram1) :pointer) ((pointer histogram2) :pointer))
  :export nil
  :index gsl/
  :documentation
  "Divide the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. @math{h'_1(i) = h_1(i) / h_2(i)}.  The two histograms
   must have identical bin ranges.")

(defmethod gsl/ ((histogram1 histogram) (histogram2 histogram))
  "Divide the contents of the bins of histogram1 by the
   contents of the corresponding bins in histogram2
   i.e. @math{h'_1(i) = h_1(i) / h_2(i)}.  The two histograms
   must have identical bin ranges."
  (histo-1d2d histogram1 gsl/ (histogram2)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl scale-1 (histogram scale)
  "gsl_histogram_scale"
  (((pointer histogram) :pointer) (scale :double))
  :export nil
  :index scale
  :documentation
  "Multiply the contents of the bins of histogram by the
   constant scale, i.e. h'_1(i) = h_1(i) * scale.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl scale-2 (histogram scale)
  "gsl_histogram2d_scale"
  (((pointer histogram) :pointer) (scale :double))
  :export nil
  :index scale
  :documentation
  "Multiply the contents of the bins of histogram by the
   constant scale, i.e. h'_1(i) = h_1(i) * scale.")

(export 'scale)
(defun scale (histogram scale)
  "Multiply the contents of the bins of histogram by the
   constant scale, i.e. h'_1(i) = h_1(i) * scale."
  (histo-1d2d histogram scale (scale)))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl shift-1 (histogram offset)
  "gsl_histogram_shift"
  (((pointer histogram) :pointer) (offset :double))
  :export nil
  :index shift
  :documentation
  "Shift the contents of the bins of histogram @var{h} by the
   constant offset, i.e. h'_1(i) = h_1(i) + offset.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl shift-2 (histogram offset)
  "gsl_histogram2d_shift"
  (((pointer histogram) :pointer) (offset :double))
  :export nil
  :index shift
  :documentation
  "Shift the contents of the bins of histogram @var{h} by the
   constant offset, i.e. h'_1(i) = h_1(i) + offset.")

(export 'shift)
(defun shift (histogram offset)
  (histo-1d2d histogram shift (offset)))
