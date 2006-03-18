;********************************************************
; file:        airy.lisp                                 
; description: Airy functions                            
; date:        Fri Mar 17 2006 - 18:41                   
; author:      Liam M. Healy
; modified:    Sat Mar 18 2006 - 00:21
;********************************************************

(in-package :gsl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '()))

;;;;****************************************************************************
;;;; Airy functions
;;;;****************************************************************************

(defun-sf airy-ai ((x :double))
  "gsl_sf_airy_Ai_e"
  :return (sf-result)
  :documentation "The Airy function Ai(x)."
  :mode t)

;;; (airy-ai 0.45d0)
;;; 0.24308135437540998d0
;;; 6.288670879282072d-17

(defun-sf airy-bi ((x :double))
  "gsl_sf_airy_Bi_e"
  :return (sf-result)
  :documentation "The Airy function Bi(x)."
  :mode t)

(defun-sf airy-ai-scaled ((x :double))
  "gsl_sf_airy_Ai_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function @math{S_A(x) Ai(x)}.  For @math{x>0} the scaling factor @math{S_A(x)} is @c{$\exp(+(2/3) x^{3/2})$} @math{\exp(+(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-sf airy-bi-scaled ((x :double))
  "gsl_sf_airy_Bi_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function @math{S_B(x) Bi(x)}.  For @math{x>0} the scaling factor @math{S_B(x)} is @c{$\exp(-(2/3) x^{3/2})$} @math{exp(-(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-sf airy-ai-deriv ((x :double))
  "gsl_sf_airy_Ai_deriv_e"
  :return (sf-result)
  :documentation "The Airy function derivative Ai'(x)."
  :mode t)

(defun-sf airy-bi-deriv ((x :double))
  "gsl_sf_airy_Bi_deriv_e"
  :return (sf-result)
  :documentation "The Airy function derivative Bi'(x)."
  :mode t)

(defun-sf airy-ai-deriv-scaled ((x :double))
  "gsl_sf_airy_Ai_deriv_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function derivative S_A(x) Ai'(x).  For @math{x>0} the scaling factor @math{S_A(x)} is @c{$\exp(+(2/3) x^{3/2})$} @math{\exp(+(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-sf airy-bi-deriv-scaled ((x :double))
  "gsl_sf_airy_Bi_deriv_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function derivative S_B(x) Bi'(x).  For @math{x>0} the scaling factor @math{S_B(x)} is @c{$\exp(-(2/3) x^{3/2})$} @math{exp(-(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-sf airy-zero-ai ((s :unsigned-int))
  "gsl_sf_airy_zero_Ai_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function @math{Ai(x)}."
  :mode t)

(defun-sf airy-zero-bi ((s :unsigned-int))
  "gsl_sf_airy_zero_Bi_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function @math{Bi(x)}."
  :mode t)

(defun-sf airy-zero-ai-deriv ((s :unsigned-int))
  "gsl_sf_airy_zero_Ai_deriv_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy
function derivative @math{Ai'(x)}."
  :mode t)

(defun-sf airy-zero-bi-deriv ((s :unsigned-int))
  "gsl_sf_airy_zero_Bi_deriv_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function derivative @math{Bi'(x)}."
  :mode t)

