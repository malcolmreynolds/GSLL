;********************************************************
; file:        zeta.lisp                         
; description: Zeta functions                              
; date:        Sat May 13 2006 - 23:27
; author:      Liam M. Healy                             
; modified:    Sat May 13 2006 - 23:52
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Riemann Zeta Function
;;;;****************************************************************************

;;; The Riemann zeta function is defined by the infinite sum 
;;; @math{\zeta(s) = \sum_@{k=1@}^\infty k^@{-s@}}.  

(defgeneric zeta (x)
  (:documentation "The Riemann zeta function @math{\zeta(n)}."))

(defun-gsl zeta ((n :int))
  "gsl_sf_zeta_int_e"
  :documentation "The Riemann zeta function @math{\zeta(n)} 
   for integer @var{n}, @math{n \ne 1}."
  :method ((n fixnum))
  :return (sf-result))

(defun-gsl zeta ((s :double))
  "gsl_sf_zeta_e"
  :documentation "The Riemann zeta function @math{\zeta(s)}
   for arbitrary @var{s}, @math{s \ne 1}."
  :method ((s double-float))
  :return (sf-result))

;;;;****************************************************************************
;;;; Riemann Zeta Function Minus One
;;;;****************************************************************************

(defgeneric zeta-1 (x)
  (:documentation "zeta - 1."))

(defun-gsl zeta-1 ((n :int))
  "gsl_sf_zetam1_int_e"
  :documentation "The Riemann zeta function @math{\zeta(n)} 
   for integer @var{n}, @math{n \ne 1}."
  :method ((n fixnum))
  :return (sf-result))

(defun-gsl zeta-1 ((s :double))
  "gsl_sf_zetam1_e"
  :documentation "The Riemann zeta function @math{\zeta(s)}
   for arbitrary @var{s}, @math{s \ne 1}."
  :method ((s double-float))
  :return (sf-result))

;;;;****************************************************************************
;;;; Hurwitz Zeta Function
;;;;****************************************************************************

(defun-gsl hurwitz-zeta ((s :double) (q :double))
  "gsl_sf_hzeta_e"
  :documentation "The Hurwitz zeta function @math{\zeta(s,q)} for
  @math{s > 1}, @math{q > 0}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Eta Function
;;;;****************************************************************************

(defun-gsl eta ((s :double))
  "gsl_sf_eta_e"
  :documentation "The eta function @math{\eta(s)} for arbitrary @var{s}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test zeta
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.164493406685d+01" (ZETA 2))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.261237534869d+01" (ZETA 1.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.644934066848d+00" (ZETA-1 2))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.161237534869d+01" (ZETA-1 1.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.140377976886d+01"
				   (HURWITZ-ZETA 1.5d0 2.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.765147024625d+00" (ETA 1.5d0)))
