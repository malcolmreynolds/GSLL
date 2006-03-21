;********************************************************
; file:        elliptic-functions.lisp
; description: Jacobian elliptic functions                        
; date:        Mon Mar 20 2006 - 22:21                   
; author:      Liam M. Healy                             
; modified:    Mon Mar 20 2006 - 23:11
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-sf jacobian-elliptic-functions ((u :double) (m :double))
  "gsl_sf_elljac_e"
  :documentation
  "The Jacobian elliptic functions @math{sn(u|m)},
@math{cn(u|m)}, @math{dn(u|m)} computed by descending Landen
transformations."
  :return (:double :double :double))

;;; > (jacobian-elliptic-functions 0.61802d0 0.5d0)
;;; 0.564575752943391
;;; 0.8253812568676386
;;; 0.916857191493965
;;; > (jacobian-elliptic-functions 0.2d0 0.81d0)
;;; 0.19762082367187697
;;; 0.9802785369736752
;;; 0.9840560289645665
;;; > (jacobian-elliptic-functions 0.61802d0 1.5d0)
;;; ;;;error
