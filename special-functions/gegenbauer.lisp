;; Gegenbauer polynomials
;; Liam Healy, Fri Apr 28 2006 - 20:40
;; Time-stamp: <2008-12-26 12:25:31EST gegenbauer.lisp>
;; $Id$

(in-package :gsl)

(defmfun gegenbauer-1 (lambda x)
  "gsl_sf_gegenpoly_1_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Gegenbauer polynomial C^{(\lambda)}_1(x)}.")

(defmfun gegenbauer-2 (lambda x)
  "gsl_sf_gegenpoly_2_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Gegenbauer polynomial C^{(\lambda)}_2(x)}.")

(defmfun gegenbauer-3 (lambda x)
  "gsl_sf_gegenpoly_3_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Gegenbauer polynomial C^{(\lambda)}_3(x)}.")

(defmfun gegenbauer (n lambda x)
  "gsl_sf_gegenpoly_n_e"
  ((n :int) (lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Gegenbauer polynomial C^{(\lambda)}_n(x)} for a specific value of n,
  lambda, x subject to \lambda > -1/2, n >= 0.")

(defmfun gegenbauer-array (lambda x result)
  "gsl_sf_gegenpoly_array"
  (((1- (dim0 result)) :int)
   (lambda :double) (x :double) ((c-pointer result) :pointer))
  :outputs (result)
  :documentation			; FDL
  "Compute an array of Gegenbauer polynomials C^{(\lambda)}_n(X)}
   for n = 0, 1, 2, ..., length(result)-1}, subject to \lambda > -1/2.")

;;; (defparameter vec (make-data 'vector nil 3))
;;; (gegenbauer-array 1.0d0 3.0d0 vec)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test gegenbauer
  (gegenbauer-1 1.0d0 3.0d0)
  (gegenbauer-2 1.0d0 3.0d0)
  (gegenbauer-3 1.0d0 3.0d0)
  (gegenbauer 4 1.0d0 3.0d0)
  (let ((arr (make-marray 'double-float :dimensions 4)))
      (gegenbauer-array 1.0d0 3.0d0 arr) (cl-array arr)))
