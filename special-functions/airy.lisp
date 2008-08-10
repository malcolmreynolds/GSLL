;; Airy functions
;; Liam Healy, Fri Mar 17 2006 - 18:41
;; Time-stamp: <2008-08-10 17:50:02EDT airy.lisp>
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

#|
(make-tests airy
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
|#

(LISP-UNIT:DEFINE-TEST AIRY
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(0.01572592338047048d0 2.1573014423586447d-17)
   (MULTIPLE-VALUE-LIST (AIRY-AI 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(6.48166073846058d0 8.77836191730236d-15)
   (MULTIPLE-VALUE-LIST (AIRY-BI 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(0.21932220512871203d0 5.966864198455728d-17)
   (MULTIPLE-VALUE-LIST (AIRY-AI-SCALED 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(0.4647504801960925d0 1.1831869152362144d-16)
   (MULTIPLE-VALUE-LIST (AIRY-BI-SCALED 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(-0.02625088103590322d0 4.306971270221159d-17)
   (MULTIPLE-VALUE-LIST (AIRY-AI-DERIV 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(9.421423317334305d0 1.5213125884867257d-14)
   (MULTIPLE-VALUE-LIST (AIRY-BI-DERIV 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(-0.36610893847516224d0 1.167515239400716d-16)
   (MULTIPLE-VALUE-LIST (AIRY-AI-DERIV-SCALED 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(0.6755384441644995d0 1.978922049880242d-16)
   (MULTIPLE-VALUE-LIST (AIRY-BI-DERIV-SCALED 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(-2.338107410459767d0 5.19164136227827d-16)
   (MULTIPLE-VALUE-LIST (AIRY-ZERO-AI 1)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(-1.173713222709128d0 2.606166888317336d-16)
   (MULTIPLE-VALUE-LIST (AIRY-ZERO-BI 1)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(-1.018792971647471d0 2.2621748288986134d-16)
   (MULTIPLE-VALUE-LIST (AIRY-ZERO-AI-DERIV 1)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   '(-2.294439682614123d0 5.094679528503672d-16)
   (MULTIPLE-VALUE-LIST (AIRY-ZERO-BI-DERIV 1))))


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
