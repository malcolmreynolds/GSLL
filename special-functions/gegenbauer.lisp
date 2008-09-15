;; Gegenbauer polynomials
;; Liam Healy, Fri Apr 28 2006 - 20:40
;; Time-stamp: <2008-09-14 21:38:13EDT gegenbauer.lisp>
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

#|
(make-tests gegenbauer
  (gegenbauer-1 1.0d0 3.0d0)
  (gegenbauer-2 1.0d0 3.0d0)
  (gegenbauer-3 1.0d0 3.0d0)
  (gegenbauer 4 1.0d0 3.0d0)
  (letm ((arr (vector-double-float 4)))
      (gegenbauer-array 1.0d0 3.0d0 arr) (cl-array arr)))
|#

(LISP-UNIT:DEFINE-TEST GEGENBAUER
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 6.0d0 5.329070518200751d-15)
   (MULTIPLE-VALUE-LIST (GEGENBAUER-1 1.0d0 3.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 35.0d0 1.5765166949677223d-14)
   (MULTIPLE-VALUE-LIST (GEGENBAUER-2 1.0d0 3.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 204.0d0 9.126033262418787d-14)
   (MULTIPLE-VALUE-LIST (GEGENBAUER-3 1.0d0 3.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1189.0d0 1.056044141023449d-12)
   (MULTIPLE-VALUE-LIST (GEGENBAUER 4 1.0d0 3.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(1.0d0 6.0d0 35.0d0 204.0d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((ARR (VECTOR-DOUBLE-FLOAT 4)))
      (GEGENBAUER-ARRAY 1.0d0 3.0d0 ARR) (CL-ARRAY ARR)))))
