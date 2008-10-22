;; Airy functions
;; Liam Healy, Fri Mar 17 2006 - 18:41
;; Time-stamp: <2008-10-20 22:27:27EDT airy.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Airy functions
;;;;****************************************************************************

(defmfun airy-Ai (x &optional (mode :double))
    "gsl_sf_airy_Ai_e" ((x :double) (mode sf-mode) (ret sf-result))
  :documentation			; FDL
  "The Airy function Ai(x).")

(defmfun airy-Bi (x &optional (mode :double))
  "gsl_sf_airy_Bi_e" ((x :double) (mode sf-mode) (ret sf-result))
  :documentation			; FDL
  "The Airy function Bi(x).")

(defmfun airy-Ai-scaled (x &optional (mode :double))
  "gsl_sf_airy_Ai_scaled_e" ((x :double) (mode sf-mode) (ret sf-result))
  :documentation			; FDL
  "The scaled Airy function S_A(x) Ai(x).
   For x>0 the scaling factor S_A(x) is \exp(+(2/3) x^(3/2)),
   and is 1 for x<0.")

(defmfun airy-Bi-scaled (x &optional (mode :double))
  "gsl_sf_airy_Bi_scaled_e" ((x :double) (mode sf-mode) (ret sf-result))
  :documentation			; FDL
  "The scaled Airy function S_B(x) Bi(x).
   For x>0 the scaling factor S_B(x) is exp(-(2/3) x^(3/2)),
   and is 1 for x<0.")

(defmfun airy-Ai-deriv (x &optional (mode :double))
  "gsl_sf_airy_Ai_deriv_e" ((x :double) (mode sf-mode) (ret sf-result))
  :documentation			; FDL
  "The Airy function derivative Ai'(x).")

(defmfun airy-Bi-deriv (x &optional (mode :double))
  "gsl_sf_airy_Bi_deriv_e" ((x :double) (mode sf-mode) (ret sf-result))
  :documentation "The Airy function derivative Bi'(x).")

(defmfun airy-Ai-deriv-scaled (x &optional (mode :double))
  "gsl_sf_airy_Ai_deriv_scaled_e" ((x :double) (mode sf-mode) (ret sf-result))
  :documentation			; FDL
  "The scaled Airy function derivative S_A(x) Ai'(x).
  For x>0 the scaling factor S_A(x) is exp(+(2/3) x^(3/2)),
  and is 1 for x<0.")

(defmfun airy-Bi-deriv-scaled (x &optional (mode :double))
  "gsl_sf_airy_Bi_deriv_scaled_e" ((x :double) (mode sf-mode) (ret sf-result))
  :documentation			; FDL
  "The scaled Airy function derivative S_B(x) Bi'(x).
   For x>0 the scaling factor S_B(x) is
   exp(-(2/3) x^(3/2)), and is 1 for x<0.")

(defmfun airy-zero-Ai (s)
  "gsl_sf_airy_zero_Ai_e" ((s sizet) (ret sf-result))
  :documentation			; FDL
  "The location of the s-th zero of the Airy function Ai(x).")

(defmfun airy-zero-Bi (s)
  "gsl_sf_airy_zero_Bi_e" ((s sizet) (ret sf-result))
  :documentation			; FDL
  "The location of the s-th zero of the Airy function Bi(x).")

(defmfun airy-zero-Ai-deriv (s)
  "gsl_sf_airy_zero_Ai_deriv_e" ((s sizet) (ret sf-result))
  :documentation			; FDL
  "The location of the s-th zero of the Airy function derivative Ai'(x).")

(defmfun airy-zero-Bi-deriv (s)
  "gsl_sf_airy_zero_Bi_deriv_e" ((s sizet) (ret sf-result))
  :documentation			; FDL
  "The location of the s-th zero of the Airy function derivative Bi'(x).")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test airy
  (airy-ai 2.5d0)
  (airy-bi 2.5d0)
  (airy-ai-scaled 2.5d0)
  (airy-bi-scaled 2.5d0)
  (airy-ai-deriv 2.5d0)
  (airy-bi-deriv 2.5d0)
  (airy-ai-deriv-scaled 2.5d0)
  (airy-bi-deriv-scaled 2.5d0)
  (airy-zero-ai 1)
  (airy-zero-bi 1)
  (airy-zero-ai-deriv 1)
  (airy-zero-bi-deriv 1))

#|
;;; Mathematica results
In[4]:= AiryAi[2.5]
Out[4]= 0.01572592338047049
In[5]:= AiryBi[2.5]
Out[5]= 6.481660738460579
In[6]:= AiryAiPrime[2.5]
Out[6]= -0.02625088103590323
In[7]:= AiryBiPrime[2.5]
Out[7]= 9.4214233173343
|#
