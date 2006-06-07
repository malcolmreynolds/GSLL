;********************************************************
; file:        airy.lisp                                 
; description: Airy functions                            
; date:        Fri Mar 17 2006 - 18:41                   
; author:      Liam M. Healy
; modified:    Wed Jun  7 2006 - 17:32
;********************************************************

(in-package :gsl)

;;;;****************************************************************************
;;;; Airy functions
;;;;****************************************************************************

(defun-gsl airy-Ai (x)
    "gsl_sf_airy_Ai_e" ((x :double) :mode (ret sf-result))
  :documentation "The Airy function Ai(x).")

(defun-gsl airy-Bi (x)
  "gsl_sf_airy_Bi_e" ((x :double) :mode (ret sf-result))
  :documentation "The Airy function Bi(x).")

(defun-gsl airy-Ai-scaled (x)
  "gsl_sf_airy_Ai_scaled_e" ((x :double) :mode (ret sf-result))
  :documentation
  "The scaled Airy function @math{S_A(x) Ai(x)}.
   For @math{x>0} the scaling factor @math{S_A(x)} is @math{\exp(+(2/3) x^(3/2))},
   and is 1 for @math{x<0}.")

(defun-gsl airy-Bi-scaled (x)
  "gsl_sf_airy_Bi_scaled_e" ((x :double) :mode (ret sf-result))
  :documentation "The scaled Airy function @math{S_B(x) Bi(x)}.
   For @math{x>0} the scaling factor @math{S_B(x)} is @math{exp(-(2/3) x^(3/2))},
   and is 1 for @math{x<0}.")

(defun-gsl airy-Ai-deriv (x)
  "gsl_sf_airy_Ai_deriv_e" ((x :double) :mode (ret sf-result))
  :documentation "The Airy function derivative Ai'(x).")

(defun-gsl airy-Bi-deriv (x)
  "gsl_sf_airy_Bi_deriv_e" ((x :double) :mode (ret sf-result))
  :documentation "The Airy function derivative Bi'(x).")

(defun-gsl airy-Ai-deriv-scaled (x)
  "gsl_sf_airy_Ai_deriv_scaled_e" ((x :double) :mode (ret sf-result))
  :documentation "The scaled Airy function derivative S_A(x) Ai'(x).
  For @math{x>0} the scaling factor @math{S_A(x)} is @math{\exp(+(2/3) x^(3/2))},
  and is 1 for @math{x<0}.")

(defun-gsl airy-Bi-deriv-scaled (x)
  "gsl_sf_airy_Bi_deriv_scaled_e" ((x :double) :mode (ret sf-result))
  :documentation "The scaled Airy function derivative S_B(x) Bi'(x).
   For @math{x>0} the scaling factor @math{S_B(x)} is
   @math{exp(-(2/3) x^(3/2))}, and is 1 for @math{x<0}.")

(defun-gsl airy-zero-Ai (s)
  "gsl_sf_airy_zero_Ai_e" ((s :size) (ret sf-result))
  :documentation "The location of the @var{s}-th zero of the Airy
  function @math{Ai(x)}.")

(defun-gsl airy-zero-Bi (s)
  "gsl_sf_airy_zero_Bi_e" ((s :size) (ret sf-result))
  :documentation "The location of the @var{s}-th zero of the Airy
   function @math{Bi(x)}.")

(defun-gsl airy-zero-Ai-deriv (s)
  "gsl_sf_airy_zero_Ai_deriv_e" ((s :size) (ret sf-result))
  :documentation "The location of the @var{s}-th zero of the Airy
   function derivative @math{Ai'(x)}.")

(defun-gsl airy-zero-Bi-deriv (s)
  "gsl_sf_airy_zero_Bi_deriv_e" ((s :size) (ret sf-result))
  :documentation "The location of the @var{s}-th zero of the Airy
   function derivative @math{Bi'(x)}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test airy
  (lisp-unit:assert-first-fp-equal "0.157259233805d-01" (airy-ai 2.5d0))
  (lisp-unit:assert-first-fp-equal "0.648166073846d+01" (airy-bi 2.5d0))
  (lisp-unit:assert-first-fp-equal "0.219322205129d+00" (airy-ai-scaled 2.5d0))
  (lisp-unit:assert-first-fp-equal "0.464750480196d+00" (airy-bi-scaled 2.5d0))
  (lisp-unit:assert-first-fp-equal "-0.262508810359d-01" (airy-ai-deriv 2.5d0))
  (lisp-unit:assert-first-fp-equal "0.942142331733d+01" (airy-bi-deriv 2.5d0))
  (lisp-unit:assert-first-fp-equal "-0.366108938475d+00"
				   (airy-ai-deriv-scaled 2.5d0))
  (lisp-unit:assert-first-fp-equal "0.675538444164d+00"
				   (airy-bi-deriv-scaled 2.5d0))
  (lisp-unit:assert-first-fp-equal "-0.233810741046d+01" (airy-zero-ai 1))
  (lisp-unit:assert-first-fp-equal "-0.117371322271d+01" (airy-zero-bi 1))
  (lisp-unit:assert-first-fp-equal "-0.101879297165d+01" (airy-zero-ai-deriv 1))
  (lisp-unit:assert-first-fp-equal "-0.229443968261d+01" (airy-zero-bi-deriv 1)))
