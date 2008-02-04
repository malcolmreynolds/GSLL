;; Gegenbauer polynomials
;; Liam Healy, Fri Apr 28 2006 - 20:40
;; Time-stamp: <2008-02-03 19:23:36EST gegenbauer.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl gegenbauer-1 (lambda x)
  "gsl_sf_gegenpoly_1_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Gegenbauer polynomial C^{(\lambda)}_1(x)}.")

(defun-gsl gegenbauer-2 (lambda x)
  "gsl_sf_gegenpoly_2_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Gegenbauer polynomial C^{(\lambda)}_2(x)}.")

(defun-gsl gegenbauer-3 (lambda x)
  "gsl_sf_gegenpoly_3_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Gegenbauer polynomial C^{(\lambda)}_3(x)}.")

(defun-gsl gegenbauer (n lambda x)
  "gsl_sf_gegenpoly_n_e"
  ((n :int) (lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Gegenbauer polynomial C^{(\lambda)}_n(x)} for a specific value of n,
  lambda, x subject to \lambda > -1/2, n >= 0.")

(defun-gsl gegenbauer-array (lambda x result)
  "gsl_sf_gegenpoly_array"
  (((1- (dim0 result)) :int)
   (lambda :double) (x :double) ((gsl-array result) :pointer))
  :invalidate (result)
  :documentation			; FDL
  "Compute an array of Gegenbauer polynomials C^{(\lambda)}_n(X)}
   for n = 0, 1, 2, ..., length(result)-1}, subject to \lambda > -1/2.")

;;; (defparameter vec (make-data 'vector nil 3))
;;; (gegenbauer-array 1.0d0 3.0d0 vec)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test gegenbauer
  (lisp-unit:assert-first-fp-equal
   "0.600000000000d+01"
   (gegenbauer-1 1.0d0 3.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.350000000000d+02"
   (gegenbauer-2 1.0d0 3.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.204000000000d+03"
   (gegenbauer-3 1.0d0 3.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.118900000000d+04"
   (gegenbauer 4 1.0d0 3.0d0))
  (lisp-unit:assert-equal
   '("0.100000000000d+01" "0.600000000000d+01" "0.350000000000d+02"
     "0.204000000000d+03")
   (lisp-unit:fp-sequence
    (letm ((arr (vector-double 4)))
      (gegenbauer-array 1.0d0 3.0d0 arr) (data arr)))))
