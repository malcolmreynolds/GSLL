;********************************************************
; file:        clausen.lisp                              
; description: Clausen function                          
; date:        Sat Mar 18 2006 - 23:18                   
; author:      Liam M. Healy                             
; modified:    Sun May 21 2006 - 14:43
;********************************************************
;;; $Id:$

(in-package :gsl)

(defun-gsl clausen ((x :double))
  "gsl_sf_clausen_e"
  :documentation
  "The Clausen integral @math{Cl_2(x)}."
  :return (sf-result))

(lisp-unit:define-test clausen
  (lisp-unit:assert-first-fp-equal "0.433598203236d+00" (clausen 2.5d0)))
