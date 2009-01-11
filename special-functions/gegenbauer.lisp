;; Gegenbauer polynomials
;; Liam Healy, Fri Apr 28 2006 - 20:40
;; Time-stamp: <2009-01-11 09:45:00EST gegenbauer.lisp>
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

(defmfun gegenbauer-array
    (lambda x &optional (size-or-array *default-sf-array-size*)
	    &aux (array (vdf size-or-array)))
  "gsl_sf_gegenpoly_array"
  (((1- (dim0 array)) :int)
   (lambda :double) (x :double) ((c-pointer array) :pointer))
  :outputs (array)
  :documentation			; FDL
  "Compute an array of Gegenbauer polynomials C^{(\lambda)}_n(X)}
   for n = 0, 1, 2, ..., length(array)-1}, subject to \lambda > -1/2.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test gegenbauer
  (gegenbauer-1 1.0d0 3.0d0)
  (gegenbauer-2 1.0d0 3.0d0)
  (gegenbauer-3 1.0d0 3.0d0)
  (gegenbauer 4 1.0d0 3.0d0)
  (cl-array (gegenbauer-array 1.0d0 3.0d0 4)))
