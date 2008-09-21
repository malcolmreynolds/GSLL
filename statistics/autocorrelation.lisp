;; Autocorrelation
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-09-21 15:56:14EDT autocorrelation.lisp>
;; $Id$

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmfun autocorrelation ((data vector) &optional mean)
  (("gsl_stats" :type "_lag1_autocorrelation")
   ("gsl_stats" :type "_lag1_autocorrelation_m"))
  ((((c-pointer data) :pointer) (1 :int) ((dim0 data) sizet))
   (((c-pointer data) :pointer) (1 :int) ((dim0 data) sizet)
    (mean :double)))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "The lag-1 autocorrelation of the dataset data.
  a_1 = {\sum_{i = 1}^{n} (x_{i} - \Hat\mu) (x_{i-1} - \Hat\mu)
  \over
  \sum_{i = 1}^{n} (x_{i} - \Hat\mu) (x_{i} - \Hat\mu)}.")

;;; Examples and unit test

#|
(make-tests autocorrelation
  (letm ((vec (vector-double-float (a -3.21d0 1.0d0 12.8d0))))
      (let ((mean (mean vec)))
	(list
	 (autocorrelation vec)
	 (autocorrelation vec mean)))))
|#

(LISP-UNIT:DEFINE-TEST AUTOCORRELATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST -0.04646366834251103d0 -0.04646366834251103d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE-FLOAT (A -3.21d0 1.0d0 12.8d0))))
      (LET ((MEAN (MEAN VEC)))
	(LIST (AUTOCORRELATION VEC)
	      (AUTOCORRELATION VEC MEAN)))))))

