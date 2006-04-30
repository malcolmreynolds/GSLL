;********************************************************
; file:        lambert.lisp
; description: Lambert's W functions
; date:        Fri Apr 28 2006 - 20:40                   
; author:      Liam M. Healy                             
; modified:    Sat Apr 29 2006 - 19:00
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Lambert's W functions, @math{W(x)}, are defined to be solutions
;;; of the equation @math{W(x) \exp(W(x)) = x}. This function has
;;; multiple branches for @math{x < 0}; however, it has only
;;; two real-valued branches. We define @math{W_0(x)} to be the
;;; principal branch, where @math{W > -1} for @math{x < 0}, and 
;;; @c{$W_{-1}(x)$}
;;; @math{W_@{-1@}(x)} to be the other real branch, where
;;; @math{W < -1} for @math{x < 0}.  

(defun-gsl lambert-W0 ((x :double))
  "gsl_sf_lambert_W0_e"
  :return (sf-result)
  :documentation
  "The principal branch of the Lambert W function, @math{W_0(x)}.")

(defun-gsl lambert-Wm1 ((x :double))
  "gsl_sf_lambert_Wm1_e"
  :return (sf-result)
  :documentation
  "The secondary real-valued branch of the Lambert W function, 
   @math{W_@{-1@}(x)}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test lambert
  (lisp-unit:assert-first-fp-equal
   "0.567143290410d+00"
   (lambert-W0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.567143290410d+00"
   (lambert-Wm1 1.0d0)))
