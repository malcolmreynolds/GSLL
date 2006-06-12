;********************************************************
; file:        clausen.lisp                              
; description: Clausen function                          
; date:        Sat Mar 18 2006 - 23:18                   
; author:      Liam M. Healy                             
; modified:    Sun Jun 11 2006 - 21:03
;********************************************************
;;; $Id:$

(in-package :gsl)

(defun-gsl clausen (x)
  "gsl_sf_clausen_e" ((x :double) (ret sf-result))
  :documentation
  "The Clausen integral @math{Cl_2(x)}.")

(lisp-unit:define-test clausen
  (lisp-unit:assert-first-fp-equal "0.433598203236d+00" (clausen 2.5d0)))
