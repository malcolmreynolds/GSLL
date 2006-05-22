;********************************************************
; file:        airy.lisp                                 
; description: Airy functions                            
; date:        Fri Mar 17 2006 - 18:41                   
; author:      Liam M. Healy
; modified:    Sun May 21 2006 - 14:09
;********************************************************

(in-package :gsl)

;;; airy-zero-Ai, airy-zero-Bi seem to only get memory fault

;;;;****************************************************************************
;;;; Airy functions
;;;;****************************************************************************

(defun-gsl airy-Ai ((x :double))
  "gsl_sf_airy_Ai_e"
  :return (sf-result)
  :documentation "The Airy function Ai(x)."
  :mode t)

;;; (airy-ai 0.45d0)
;;; 0.24308135437540998d0
;;; 6.288670879282072d-17

(defun-gsl airy-Bi ((x :double))
  "gsl_sf_airy_Bi_e"
  :return (sf-result)
  :documentation "The Airy function Bi(x)."
  :mode t)

(defun-gsl airy-Ai-scaled ((x :double))
  "gsl_sf_airy_Ai_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function @math{S_A(x) Ai(x)}.  For @math{x>0} the scaling factor @math{S_A(x)} is @math{\exp(+(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-gsl airy-Bi-scaled ((x :double))
  "gsl_sf_airy_Bi_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function @math{S_B(x) Bi(x)}.  For @math{x>0} the scaling factor @math{S_B(x)} is @c{$\exp(-(2/3) x^{3/2})$} @math{exp(-(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-gsl airy-Ai-deriv ((x :double))
  "gsl_sf_airy_Ai_deriv_e"
  :return (sf-result)
  :documentation "The Airy function derivative Ai'(x)."
  :mode t)

(defun-gsl airy-Bi-deriv ((x :double))
  "gsl_sf_airy_Bi_deriv_e"
  :return (sf-result)
  :documentation "The Airy function derivative Bi'(x)."
  :mode t)

(defun-gsl airy-Ai-deriv-scaled ((x :double))
  "gsl_sf_airy_Ai_deriv_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function derivative S_A(x) Ai'(x).  For @math{x>0} the scaling factor @math{S_A(x)} is @c{$\exp(+(2/3) x^{3/2})$} @math{\exp(+(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-gsl airy-Bi-deriv-scaled ((x :double))
  "gsl_sf_airy_Bi_deriv_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function derivative S_B(x) Bi'(x).  For @math{x>0} the scaling factor @math{S_B(x)} is @c{$\exp(-(2/3) x^{3/2})$} @math{exp(-(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

;;; Memory fault
(defun-gsl airy-zero-Ai ((s :size))
  "gsl_sf_airy_zero_Ai_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function @math{Ai(x)}."
  :mode t)

;;; Memory fault
(defun-gsl airy-zero-Bi ((s :unsigned-int))
  "gsl_sf_airy_zero_Bi_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function @math{Bi(x)}."
  :mode t)

(defun-gsl airy-zero-Ai-deriv ((s :unsigned-int))
  "gsl_sf_airy_zero_Ai_deriv_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy
function derivative @math{Ai'(x)}."
  :mode t)

(defun-gsl airy-zero-Bi-deriv ((s :unsigned-int))
  "gsl_sf_airy_zero_Bi_deriv_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function derivative @math{Bi'(x)}."
  :mode t)

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
				   (airy-bi-deriv-scaled 2.5d0)))
