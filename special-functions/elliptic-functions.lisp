;********************************************************
; file:        elliptic-functions.lisp
; description: Jacobian elliptic functions                        
; date:        Mon Mar 20 2006 - 22:21                   
; author:      Liam M. Healy                             
; modified:    Tue Jun 13 2006 - 21:16
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl jacobian-elliptic-functions (u m)
  "gsl_sf_elljac_e"
  ((u :double) (m :double) (sn sf-result) (cn sf-result) (dn sf-result))
  :documentation
  "The Jacobian elliptic functions @math{sn(u|m)},
  @math{cn(u|m)}, @math{dn(u|m)} computed by descending Landen
  transformations."
  :return ((val sn) (val cn) (val dn) (err sn) (err cn) (err dn)))

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


(lisp-unit:define-test elliptic-functions
  (lisp-unit:assert-equal
   '("0.197620823672d+00" "0.980278536974d+00" "0.984056028965d+00")
   (subseq
    (lisp-unit:fp-values (jacobian-elliptic-functions 0.2d0 0.81d0))
    0 3))
  (lisp-unit:assert-error
   'gsl-error
   (jacobian-elliptic-functions 0.61802d0 1.5d0)))
