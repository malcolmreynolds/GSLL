;; Covariance
;; Liam Healy, Sun Dec 31 2006 - 13:19
;; Time-stamp: <2008-02-17 16:48:17EST covariance.lisp>
;; $Id$

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defmfun covariance-nom (data1 data2)
  "gsl_stats_covariance"
  (((gsl-array data1) :pointer) (1 :int)
   ((gsl-array data2) :pointer) (1 :int) ((dim0 data2) size))
  :c-return :double
  :index covariance
  :export nil)

(defmfun covariance-m (data1 data2 mean1 mean2)
  "gsl_stats_covariance_m"
  (((gsl-array data1) :pointer) (1 :int)
   ((gsl-array data2) :pointer) (1 :int) ((dim0 data2) size)
   (mean1 :double) (mean2 :double))
  :c-return :double
  :index covariance
  :export nil)

(export 'covariance)
(defun-optionals covariance (data1 data2 &optional mean1 mean2)
  -nom -m
  ;; FDL
  "The covariance of the datasets data1 and data2 which must
   be of the same length,
   covar = {1 \over (n - 1)} \sum_{i = 1}^{n}
      (x_{i} - \Hat x) (y_{i} - \Hat y).")

;;; Examples and unit test

#|
(make-tests covariance
  (letm ((vec1 (vector-double #(-3.21d0 1.0d0 12.8d0)))
	   (vec2 (vector-double #(1.15d0 -1.0d0 0.5d0))))
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
	((VEC1 (VECTOR-DOUBLE #(-3.21d0 1.0d0 12.8d0)))
	 (VEC2 (VECTOR-DOUBLE #(1.15d0 -1.0d0 0.5d0))))
      (LET ((MEAN1 (MEAN VEC1)) (MEAN2 (MEAN VEC2)))
	(LIST (COVARIANCE VEC1 VEC2)
	      (COVARIANCE VEC1 VEC2 MEAN1 MEAN2)))))))

