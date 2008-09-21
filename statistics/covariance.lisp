;; Covariance
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-09-21 15:57:06EDT covariance.lisp>
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
  :documentation			; FDL
  "The covariance of the datasets data1 and data2 which must
   be of the same length,
   covar = {1 \over (n - 1)} \sum_{i = 1}^{n}
      (x_{i} - \Hat x) (y_{i} - \Hat y).")

;;; Examples and unit test

#|
(make-tests covariance
  (letm ((vec1 (vector-double-float (a -3.21d0 1.0d0 12.8d0)))
	   (vec2 (vector-double-float (a 1.15d0 -1.0d0 0.5d0))))
      (let ((mean1 (mean vec1))
	    (mean2 (mean vec2)))
	(list
	 (covariance vec1 vec2)
	 (covariance vec1 vec2 mean1 mean2)))))
|#

(LISP-UNIT:DEFINE-TEST COVARIANCE
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST -0.2929999999999998d0 -0.2929999999999998d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((VEC1 (VECTOR-DOUBLE-FLOAT (a -3.21d0 1.0d0 12.8d0)))
	 (VEC2 (VECTOR-DOUBLE-FLOAT (a 1.15d0 -1.0d0 0.5d0))))
      (LET ((MEAN1 (MEAN VEC1)) (MEAN2 (MEAN VEC2)))
	(LIST (COVARIANCE VEC1 VEC2)
	      (COVARIANCE VEC1 VEC2 MEAN1 MEAN2)))))))
