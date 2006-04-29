;********************************************************
; file:        gegenbauer.lisp                           
; description: Gegenbauer polynomials                    
; date:        Fri Apr 28 2006 - 20:40                   
; author:      Liam M. Healy                             
; modified:    Fri Apr 28 2006 - 22:44
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl gegenbauer-1 ((lambda :double) (x :double))
  "gsl_sf_gegenpoly_1_e"
  :return (sf-result)
  :documentation "The Gegenbauer polynomial @math{C^@{(\lambda)@}_1(x)}.")

(defun-gsl gegenbauer-2 ((lambda :double) (x :double))
  "gsl_sf_gegenpoly_2_e"
  :return (sf-result)
  :documentation "The Gegenbauer polynomial @math{C^@{(\lambda)@}_2(x)}.")

(defun-gsl gegenbauer-3 ((lambda :double) (x :double))
  "gsl_sf_gegenpoly_3_e"
  :return (sf-result)
  :documentation "The Gegenbauer polynomial @math{C^@{(\lambda)@}_3(x)}.")

(defun-gsl gegenbauer ((n :int) (lambda :double) (x :double))
  "gsl_sf_gegenpoly_n_e"
  :return (sf-result)
  :documentation "The Gegenbauer polynomial
  @math{C^@{(\lambda)@}_n(x)} for a specific value of @var{n},
  @var{lambda}, @var{x} subject to @math{\lambda > -1/2},
  @math{n >= 0}.")

(defun-gsl gegenbauer-array
    (((dim0 result) :int)
     (lambda :double) (x :double) ((gsl-array result) :pointer))
  "gsl_sf_gegenpoly_array"
  :function (lambda x result)
  :documentation "Compute an array of Gegenbauer polynomials
  @math{C^@{(\lambda)@}_n(x)} for @math{n = 0, 1, 2, \dots, nmax}, subject
  to @math{\lambda > -1/2}, @math{nmax >= 0}."
  :after ((cl-invalidate result))
  :return-input (result))

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
   (gegenbauer-3 1.0d0 3.0d0)))
