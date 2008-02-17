;; Lambert's W functions
;; Liam Healy, Fri Apr 28 2006 - 20:40
;; Time-stamp: <2008-02-16 22:19:40EST lambert.lisp>
;; $Id: $

(in-package :gsl)

;;; FDL
;;; Lambert's W functions, W(x), are defined to be solutions
;;; of the equation W(x) exp(W(x)) = x. This function has
;;; multiple branches for x < 0; however, it has only
;;; two real-valued branches. We define W_0(x) to be the
;;; principal branch, where W > -1 for x < 0, and 
;;; W_{-1}(x)
;;; W_{-1}(x) to be the other real branch, where
;;; W < -1 for x < 0.  

(defmfun lambert-W0 (x)
  "gsl_sf_lambert_W0_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The principal branch of the Lambert W function, W_0(x).")

(defmfun lambert-Wm1 (x)
  "gsl_sf_lambert_Wm1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The secondary real-valued branch of the Lambert W function, 
   W_{-1}(x).")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests lambert
  (lambert-W0 1.0d0)
  (lambert-Wm1 1.0d0))
|#

(LISP-UNIT:DEFINE-TEST LAMBERT
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5671432904097838d0 2.518622157098455d-16)
   (MULTIPLE-VALUE-LIST (LAMBERT-W0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5671432904097838d0 2.518622157098455d-16)
   (MULTIPLE-VALUE-LIST (LAMBERT-WM1 1.0d0))))
