;********************************************************
; file:        clausen.lisp                              
; description: Clausen function                          
; date:        Sat Mar 18 2006 - 23:18                   
; author:      Liam M. Healy                             
; modified:    Sat Mar 25 2006 - 22:11
;********************************************************
;;; $Id:$

(in-package :gsl)

(defun-gsl clausen ((x :double))
  "gsl_sf_clausen_e"
  :documentation
  "The Clausen integral @math{Cl_2(x)}."
  :return (sf-result))
