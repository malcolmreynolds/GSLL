;; Laguerre polynomials
;; Liam Healy, Fri Apr 28 2006 - 20:40
;; Time-stamp: <2008-10-25 10:56:55EDT laguerre.lisp>
;; $Id$

(in-package :gsl)

(defmfun laguerre-1 (a x)
  "gsl_sf_laguerre_1_e" ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The generalized Laguerre polynomial L^a_1(x) using
   explicit representations.")

(defmfun laguerre-2 (a x)
  "gsl_sf_laguerre_2_e" ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The generalized Laguerre polynomial
   L^a_2(x) using explicit representations.")

(defmfun laguerre-3 (a x)
  "gsl_sf_laguerre_3_e" ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The generalized Laguerre polynomial
   L^a_3(x) using explicit representations.")

(defmfun laguerre (n a x)
  "gsl_sf_laguerre_n_e" ((n :int) (a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The generalized Laguerre polynomials L^a_n(x) for a > -1, n >= 0.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

;;; Don't slime-macroexpand-1, the last one will produce an error that
;;; shouldn't be there.

(save-test laguerre
  (laguerre-1 1.0d0 3.0d0)
  (laguerre-2 1.0d0 3.0d0)
  (laguerre-3 1.0d0 3.0d0)
  (laguerre 4 1.0d0 3.0d0))

