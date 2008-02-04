;; Autocorrelation
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-02-03 23:18:13EST autocorrelation.lisp>
;; $Id: $

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defun-gsl autocorrelation-nom (data)
  "gsl_stats_lag1_autocorrelation"
  (((gsl-array data) :pointer) (1 :int) ((dim0 data) :size))
  :c-return :double
  :index autocorrelation
  :export nil)

(defun-gsl autocorrelation-m (data mean)
  "gsl_stats_lag1_autocorrelation_m"
  (((gsl-array data) :pointer) (1 :int)
   ((dim0 data) :size) (mean :double))
  :c-return :double
  :index autocorrelation
  :export nil)

(export 'autocorrelation)
(defun-optionals autocorrelation (data &optional mean)
  -nom -m
  ;; FDL
  "The lag-1 autocorrelation of the dataset @var{data}.
  a_1 = {\sum_{i = 1}^{n} (x_{i} - \Hat\mu) (x_{i-1} - \Hat\mu)
  \over
  \sum_{i = 1}^{n} (x_{i} - \Hat\mu) (x_{i} - \Hat\mu)}.")

(lisp-unit:define-test autocorrelation
  (lisp-unit:assert-equal
   '("-0.464636683425d-01" "-0.464636683425d-01")
   (lisp-unit:fp-sequence
    (letm ((vec (vector-double #(-3.21d0 1.0d0 12.8d0))))
      (let ((mean (mean vec)))
	(list
	 (autocorrelation vec)
	 (autocorrelation vec mean)))))))
