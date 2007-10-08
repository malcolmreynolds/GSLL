;********************************************************
; file:        zeta.lisp                         
; description: Zeta functions                              
; date:        Sat May 13 2006 - 23:27
; author:      Liam M. Healy                             
; modified:    Mon Oct  8 2007 - 11:30
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

(defun-gsl zeta ((n integer))
  "gsl_sf_zeta_int_e" ((n :int) (ret sf-result))
  :type :method
  :export t
  :documentation "The Riemann zeta function @math{\zeta(n)} 
   for integer @var{n}, @math{n \ne 1}.")

(defun-gsl zeta ((s float))
  "gsl_sf_zeta_e" ((s :double) (ret sf-result))
  :documentation "The Riemann zeta function @math{\zeta(s)}
   for arbitrary @var{s}, @math{s \ne 1}."
  :type :method)

;;;;****************************************************************************
;;;; Riemann Zeta Function Minus One
;;;;****************************************************************************

(defgeneric zeta-1 (x)
  (:documentation "zeta - 1."))

(defun-gsl zeta-1 ((n integer))
  "gsl_sf_zetam1_int_e" ((n :int) (ret sf-result))
  :documentation "The Riemann zeta function @math{\zeta(n)} 
   for integer @var{n}, @math{n \ne 1}."
  :type :method
  :export t)

(defun-gsl zeta-1 ((s float))
  "gsl_sf_zetam1_e" ((s :double) (ret sf-result))
  :documentation "The Riemann zeta function @math{\zeta(s)}
   for arbitrary @var{s}, @math{s \ne 1}."
  :type :method)

;;;;****************************************************************************
;;;; Hurwitz Zeta Function
;;;;****************************************************************************

(defun-gsl hurwitz-zeta (s q)
  "gsl_sf_hzeta_e" ((s :double) (q :double) (ret sf-result))
  :documentation "The Hurwitz zeta function @math{\zeta(s,q)} for
  @math{s > 1}, @math{q > 0}.")

;;;;****************************************************************************
;;;; Eta Function
;;;;****************************************************************************

(defun-gsl eta (s)
  "gsl_sf_eta_e" ((s :double) (ret sf-result))
  :documentation "The eta function @math{\eta(s)} for arbitrary @var{s}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test zeta
  (lisp-unit:assert-first-fp-equal "0.164493406685d+01" (zeta 2))
  (lisp-unit:assert-first-fp-equal "0.261237534869d+01" (zeta 1.5d0))
  (lisp-unit:assert-first-fp-equal "0.644934066848d+00" (zeta-1 2))
  (lisp-unit:assert-first-fp-equal "0.161237534869d+01" (zeta-1 1.5d0))
  (lisp-unit:assert-first-fp-equal "0.140377976886d+01"
				   (hurwitz-zeta 1.5d0 2.5d0))
  (lisp-unit:assert-first-fp-equal "0.765147024625d+00" (eta 1.5d0)))
