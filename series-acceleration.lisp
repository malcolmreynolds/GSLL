;********************************************************
; file:        series-acceleration.lisp                  
; description: Series acceleration.                      
; date:        Wed Nov 21 2007 - 18:41                   
; author:      Liam Healy                                
; modified:    Wed Nov 21 2007 - 21:21
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Needs with- macro to allocate, initialize, and free object.

;;;;****************************************************************************
;;;; Creation and calculation of Levin series acceleration
;;;;****************************************************************************

(cffi:defcstruct levin
  "The definition of Levin series acceleration for GSL."
  (size :size)
  (position-in-array :size)
  (terms-used :size)
  (sum-plain :double)
  (q-num :pointer)
  (q-den :pointer)
  (dq-num :pointer)
  (dq-den :pointer)
  (dsum :pointer))

(defun-gsl allocate-levin (order)
  "gsl_sum_levin_u_alloc"
  ((order :size))
  :c-return :pointer
  :documentation
  "Allocate a workspace for a Levin @math{u}-transform of @var{n}
   terms.  The size of the workspace is @math{O(2n^2 + 3n)}.")

(defun-gsl free-levin (levin)
  "gsl_sum_levin_u_free"
  ((levin :pointer))
  :documentation
  "Free a previously allocated Levin acceleration.")

(defun-gsl accelerate (array levin)
  "gsl_sum_levin_u_accel"
  (((gsl-array array) :pointer) ((dim0 array) :size) (levin :pointer)
   (accelerated-sum :double) (absolute-error :double))
  :documentation
  "From the terms of a series in @var{array}, compute the extrapolated
   limit of the series using a Levin @math{u}-transform.  Additional
   working space must be provided in levin.  The extrapolated sum is
   returned with an estimate of the absolute error.  The actual
   term-by-term sum is returned in 
   @code{w->sum_plain}. The algorithm calculates the truncation error
   (the difference between two successive extrapolations) and round-off
   error (propagated from the individual terms) to choose an optimal
   number of terms for the extrapolation.")

;;;;****************************************************************************
;;;; Acceleration with error estimation from truncation
;;;;****************************************************************************

(defun-gsl allocate-levin-truncated (order)
  "gsl_sum_levin_utrunc_alloc"
  ((order :size))
  :c-return :pointer
  :documentation
  "Allocate a workspace for a Levin @math{u}-transform of @var{n}
   terms, without error estimation.  The size of the workspace is
   @math{O(3n)}.")

(defun-gsl free-levin-truncated (levin)
  "gsl_sum_levin_utrunc_free"
  ((levin :pointer))
  :documentation
  "Free a previously allocated Levin acceleration.")

(defun-gsl accelerate-truncated (array levin)
  "gsl_sum_levin_utrunc_accel"
  (((gsl-array array) :pointer) ((dim0 array) :size) (levin :pointer)
   (accelerated-sum :double) (absolute-error :double))
  :documentation
  "From the terms of a series in @var{array}, compute the extrapolated
   limit of the series using a Levin @math{u}-transform.  Additional
   working space must be provided in levin.  The extrapolated sum is
   returned with an estimate of the absolute error.  The actual
   term-by-term sum is returned in @code{w->sum_plain}. The algorithm
   terminates when the difference between two successive extrapolations
   reaches a minimum or is sufficiently small. The difference between
   these two values is used as estimate of the error and is stored in
   @var{abserr_trunc}.  To improve the reliability of the algorithm the
   extrapolated values are replaced by moving averages when calculating
   the truncation error, smoothing out any fluctuations.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************

;;; From Sec. 29.3 in the GSL manual.

(defun acceleration-example ()
  (let* ((maxterms 20)
	 (levin (allocate-levin maxterms))
	 (sum 0.0d0)
	 (zeta2 (/ (expt pi 2) 6)))
    (with-data (array vector-double maxterms)
      (dotimes (n maxterms)
	(setf (gsl-aref array n) (coerce (/ (expt (1+ n) 2)) 'double-float))
	(incf sum (gsl-aref array n)))
      (multiple-value-bind (accelerated-sum error)
	  (accelerate array levin)
	(format t "~&term-by-term sum =~f using ~d terms" sum maxterms)
	(format t "~&term-by-term sum =~f using ~d terms"
		(cffi:foreign-slot-value levin 'levin 'sum-plain)
		(cffi:foreign-slot-value levin 'levin 'terms-used))
	(format t "~&exact value     = ~f" zeta2)
	(format t "~&accelerated sum = ~f using ~d terms"
		accelerated-sum
		(cffi:foreign-slot-value levin 'levin 'terms-used))
	(format t "~&estimated error = ~f" error)
	(format t "~&actual error = ~f" (- accelerated-sum zeta2))))
    (free-levin levin)))
