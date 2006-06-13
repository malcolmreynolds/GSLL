;********************************************************
; file:        debye.lisp                                
; description: Deybe functions                           
; date:        Sun Mar 19 2006 - 14:34                   
; author:      Liam M. Healy                             
; modified:    Mon Jun 12 2006 - 23:22
;********************************************************
;;; $Id: $

#|
The Debye functions @math{D_n(x)} are defined by the following integral,
D_n(x) = n/x^n \int_0^x dt (t^n/(e^t - 1))
For further information see Abramowitz &
Stegun, Section 27.1.
|#

(in-package :gsl)

(defun-gsl debye-1 (x)
  "gsl_sf_debye_1_e" ((x :double) (ret sf-result))
  :documentation
  "The first-order Debye function @math{D_1(x) = (1/x) \int_0^x dt (t/(e^t - 1))}.")

(defun-gsl debye-2 (x)
  "gsl_sf_debye_2_e" ((x :double) (ret sf-result))
  :documentation
  "The second-order Debye function
   @math{D_2(x) = (2/x^2) \int_0^x dt (t^2/(e^t - 1))}.")

(defun-gsl debye-3 (x)
  "gsl_sf_debye_3_e" ((x :double) (ret sf-result))
  :documentation
  "The third-order Debye function
   @math{D_3(x) = (3/x^3) \int_0^x dt (t^3/(e^t - 1))}.")

(defun-gsl debye-4 (x)
  "gsl_sf_debye_4_e" ((x :double) (ret sf-result))
  :documentation
  "The fourth-order Debye function
  @math{D_4(x) = (4/x^4) \int_0^x dt (t^4/(e^t - 1))}.")

(lisp-unit:define-test debye
  (lisp-unit:assert-first-fp-equal "0.777504634112d+00" (debye-1 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.707878475628d+00" (debye-2 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.674415564078d+00" (debye-3 1.0d0))
  (lisp-unit:assert-first-fp-equal "0.654874068887d+00" (debye-4 1.0d0)))
