;********************************************************
; file:        special-functions.lisp                                
; description: GSL types                                 
; date:        Sat Mar  4 2006 - 21:07                   
; author:      Liam M. Healy
; modified:    Fri Mar 17 2006 - 18:45
;********************************************************

;;;;****************************************************************************
;;;; Dilogarithm
;;;;****************************************************************************


(defun-sf jacobian-elliptic-functions	  ; CL name
    ((u :double) (m :double))		  ; inputs
  "gsl_sf_elljac_e"			  ; GSL name
  :documentation
  "The Jacobian elliptic functions sn(u|m), cn(u|m), dn(u|m)
   computed by descending Landen transformations.
   See Abramowitz & Stegun, Chapter 16"
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
