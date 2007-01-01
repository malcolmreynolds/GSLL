;********************************************************
; file:        covariance.lisp                   
; description: Covariance
; date:        Sun Dec 31 2006 - 13:19                   
; author:      Liam M. Healy                             
; modified:    Sun Dec 31 2006 - 16:59
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; To do: stride other than 1 when that information is availble from
;;; the vector.

(defun-gsl covariance-nom (data1 data2)
  "gsl_stats_covariance"
  (((gsl-array data1) :pointer) (1 :int)
   ((gsl-array data2) :pointer) (1 :int) ((dim0 data2) :size))
  :c-return :double
  :index covariance
  :export nil)

(defun-gsl covariance-m (data1 data2 mean1 mean2)
  "gsl_stats_covariance_m"
  (((gsl-array data1) :pointer) (1 :int)
   ((gsl-array data2) :pointer) (1 :int) ((dim0 data2) :size)
   (mean1 :double) (mean2 :double))
  :c-return :double
  :index covariance
  :export nil)

(export 'covariance)
(defun-optionals covariance (data1 data2 &optional mean1 mean2)
  -nom -m
  "The covariance of the datasets @var{data1} and
   @var{data2} which must both be of the same length @var{n}.
   covar = {1 \over (n - 1)} \sum_{i = 1}^{n}
      (x_{i} - \Hat x) (y_{i} - \Hat y).")

(lisp-unit:define-test covariance
  (lisp-unit:assert-equal
   '("-0.293000000000d+00" "-0.293000000000d+00")
   (with-data (vec1 vector-double 3)
     (setf (data vec1) #(-3.21d0 1.0d0 12.8d0))
     (with-data (vec2 vector-double 3)
       (setf (data vec2) #(1.15d0 -1.0d0 0.5d0))
       (let ((mean1 (mean vec1))
	     (mean2 (mean vec2)))
	 (lisp-unit:fp-sequence
	  (list
	   (covariance vec1 vec2)
	   (covariance vec1 vec2 mean1 mean2))))))))
