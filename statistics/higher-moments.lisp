;; Skewness and kurtosis.
;; Liam Healy, Sun Dec 31 2006 - 14:20
;; Time-stamp: <2008-09-21 15:55:21EDT higher-moments.lisp>
;; $Id$

(in-package :gsl)
;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmfun skewness ((data vector) &optional mean standard-deviation)
  (("gsl_stats" :type "_skew")
   ("gsl_stats" :type "_skew_m_sd"))
  ((((c-pointer data) :pointer) (1 :int) ((dim0 data) sizet))
   (((c-pointer data) :pointer) (1 :int) ((dim0 data) sizet)
    (mean :double) (standard-deviation :double)))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "The skewness of data, defined as skew = (1/N) \sum ((x_i -
  \Hat\mu)/\Hat\sigma)^3 where x_i are the elements of the dataset
  data.  The skewness measures the asymmetry of the tails of a
  distribution.  If mean and standard deviation are supplied, compute
  skewness of the dataset data using the given values skew = (1/N)
  \sum ((x_i - mean)/sd)^3.  This is useful if you have
  already computed the mean and standard deviation of data and want to
  avoid recomputing them.")

(defmfun kurtosis ((data vector) &optional mean standard-deviation)
  (("gsl_stats" :type "_kurtosis")
   ("gsl_stats" :type "_kurtosis_m_sd"))
  ((((c-pointer data) :pointer) (1 :int) ((dim0 data) sizet))
   (((c-pointer data) :pointer) (1 :int) ((dim0 data) sizet)
    (mean :double) (standard-deviation :double)))
  :definition :generic
  :element-types :no-complex
  :c-return :double
  :documentation			; FDL
  "The kurtosis of data defined as
   kurtosis = ((1/N) \sum ((x_i - \Hat\mu)/\Hat\sigma)^4)  - 3
   The kurtosis measures how sharply peaked a distribution is,
   relative to its width.  The kurtosis is normalized to zero
   for a gaussian distribution.")

(defmfun weighted-skewness
    ((data vector) (weights vector) &optional mean standard-deviation)
  (("gsl_stats" :type "_wskew")
   ("gsl_stats" :type "_wskew_m_sd"))
  ((((c-pointer weights) :pointer) (1 :int)
    ((c-pointer data) :pointer) (1 :int)
    ((dim0 data) sizet))
   (((c-pointer weights) :pointer) (1 :int)
    ((c-pointer data) :pointer) (1 :int)
    ((dim0 data) sizet)
    (mean :double) (standard-deviation :double)))
  :definition :generic
  :element-types :float
  :c-return :double
  :documentation			; FDL
  "The weighted skewness of the dataset.
   skew = (\sum w_i ((x_i - xbar)/\sigma)^3) / (\sum w_i).")

(defmfun weighted-kurtosis
    ((data vector) (weights vector) &optional mean standard-deviation)
  (("gsl_stats" :type "_wkurtosis")
   ("gsl_stats" :type "_wkurtosis_m_sd"))
  ((((c-pointer weights) :pointer) (1 :int)
    ((c-pointer data) :pointer) (1 :int)
    ((dim0 data) sizet))
   (((c-pointer weights) :pointer) (1 :int)
    ((c-pointer data) :pointer) (1 :int)
    ((dim0 data) sizet)
    (mean :double) (standard-deviation :double)))
  :definition :generic
  :element-types :float
  :c-return :double
  :documentation			; FDL
  "The weighted kurtosis of the dataset.
   kurtosis = ((\sum w_i ((x_i - xbar)/sigma)^4) / (\sum w_i)) - 3.")

;;; Examples and unit test

#|
(make-tests higher-moments
  (letm ((vec (vector-double-float (a -3.21d0 1.0d0 12.8d0))))
      (let* ((mean (mean vec))
	     (sd (standard-deviation vec mean)))
	(list
	 (skewness vec)
	 (skewness vec mean sd)
	 (kurtosis vec)
	 (kurtosis vec mean sd)))))
|#

(LISP-UNIT:DEFINE-TEST HIGHER-MOMENTS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.2765118983985497d0 0.2765118983985497d0
	  -2.333333333333333d0 -2.333333333333333d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE-FLOAT (A -3.21d0 1.0d0 12.8d0))))
      (LET* ((MEAN (MEAN VEC))
	     (SD (STANDARD-DEVIATION VEC MEAN)))
	(LIST (SKEWNESS VEC) (SKEWNESS VEC MEAN SD)
	      (KURTOSIS VEC)
	      (KURTOSIS VEC MEAN SD)))))))
