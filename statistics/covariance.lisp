;; Covariance
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2009-02-16 22:49:00EST covariance.lisp>
;; $Id$

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmfun covariance
    ((data1 vector) (data2 vector) &optional mean1 mean2)
  (("gsl_stats" :type "_covariance")
   ("gsl_stats" :type "_covariance_m"))
  ((((c-pointer data1) :pointer) (1 :int)
    ((c-pointer data2) :pointer) (1 :int) ((dim0 data2) sizet))
   (((c-pointer data1) :pointer) (1 :int)
    ((c-pointer data2) :pointer) (1 :int) ((dim0 data2) sizet)
    (mean1 :double) (mean2 :double)))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :inputs (data1 data2)
  :documentation			; FDL
  "The covariance of the datasets data1 and data2 which must
   be of the same length,
   covar = {1 \over (n - 1)} \sum_{i = 1}^{n}
      (x_{i} - \Hat x) (y_{i} - \Hat y).")

(defmfun correlation ((data1 vector) (data2 vector))
  ("gsl_stats" :type "_correlation")
  (((c-pointer data1) :pointer) (1 :int)
    ((c-pointer data2) :pointer) (1 :int) ((dim0 data2) sizet))
  :definition :generic
  :gsl-version (1 10)
  :element-types :no-complex
  :c-return :double
  :inputs (data1 data2)
  :documentation			; FDL
  "Efficiently compute the Pearson correlation coefficient between
  the datasets data1 and data2 which must both be of the same length")

;;; Examples and unit test

(generate-all-array-tests covariance :no-complex
 (let ((v1 (array-default 3))
       (v2 (array-default 3)))
   (let ((mean1 (mean v1))
	 (mean2 (mean v2)))
     (covariance v1 v2 mean1 mean2))))

(generate-all-array-tests correlation :no-complex
 (let ((v1 (array-default 3))
       (v2 (array-default 3)))
   (correlation v1 v2)))
