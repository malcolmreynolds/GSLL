;; Autocorrelation
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-12-07 18:45:02EST autocorrelation.lisp>
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
  :inputs (data)
  :documentation			; FDL
  "The lag-1 autocorrelation of the dataset data.
  a_1 = {\sum_{i = 1}^{n} (x_{i} - \Hat\mu) (x_{i-1} - \Hat\mu)
  \over
  \sum_{i = 1}^{n} (x_{i} - \Hat\mu) (x_{i} - \Hat\mu)}.")

;;; Examples and unit test

(save-test autocorrelation
  (letm ((vec #m(-3.21d0 1.0d0 12.8d0)))
      (let ((mean (mean vec)))
	(list
	 (autocorrelation vec)
	 (autocorrelation vec mean)))))
