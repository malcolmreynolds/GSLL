;; Absolute deviation
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-08-20 22:14:51EDT absolute-deviation.lisp>
;; $Id$

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmfun absolute-deviation ((data vector) &optional mean)
  (("gsl_stats" :type "_absdev")
   ("gsl_stats" :type "_absdev_m"))
  ((((c-pointer data) :pointer) (1 :int) ((dim0 data) sizet))
   (((c-pointer data) :pointer) (1 :int) ((dim0 data) sizet)
    (mean :double)))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "The absolute deviation from the mean of data.  The absolute
  deviation from the mean is defined as absdev = (1/N) \sum |x_i -
  \Hat\mu| where x_i are the elements of the dataset data.  The
  absolute deviation from the mean provides a more robust measure of
  the width of a distribution than the variance.  If 'mean is not
  supplied, this function computes the mean of data via a call to
  #'mean.  With mean supplied, this function is useful if you have
  already computed the mean of data (and want to avoid recomputing
  it), or wish to calculate the absolute deviation relative to another
  value (such as zero, or the median).")

(defmfun weighted-absolute-deviation ((data vector) (weights vector) &optional mean)
  (("gsl_stats" :type "_wabsdev")
   ("gsl_stats" :type "_wabsdev_m"))
  ((((c-pointer weights) :pointer) (1 :int)
    ((c-pointer data) :pointer) (1 :int)
    ((dim0 data) sizet))
   (((c-pointer weights) :pointer) (1 :int)
    ((c-pointer data) :pointer) (1 :int)
    ((dim0 data) sizet) (mean :double)))
  :definition :generic
  :element-types :float
  :c-return :double
  :documentation			; FDL
  "The weighted absolute deviation from the weighted
   mean, defined as
   absdev = (\sum w_i |x_i - \Hat\mu|) / (\sum w_i).")

;;; Examples and unit test

#|
(make-tests absolute-deviation
  (letm ((vec (vector-double-float #(-3.21d0 1.0d0 12.8d0)))
	   (weights (vector-double-float #(3.0d0 1.0d0 2.0d0))))
      (let ((mean (mean vec)))
	(list
	 (absolute-deviation vec)
	 (weighted-absolute-deviation vec weights)
	 (absolute-deviation vec mean)))))

(LISP-UNIT:DEFINE-TEST ABSOLUTE-DEVIATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 6.18d0 6.647777777777779d0 6.18d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE-FLOAT #(-3.21d0 1.0d0 12.8d0)))
	   (WEIGHTS (VECTOR-DOUBLE-FLOAT #(3.0d0 1.0d0 2.0d0))))
      (LET ((MEAN (MEAN VEC)))
	(LIST (ABSOLUTE-DEVIATION VEC)
	      (WEIGHTED-ABSOLUTE-DEVIATION VEC WEIGHTS)
	      (ABSOLUTE-DEVIATION VEC MEAN)))))))

|#
