;; Clausen function
;; Liam Healy, Sat Mar 18 2006 - 23:18
;; Time-stamp: <2008-02-16 19:20:29EST clausen.lisp>
;; $Id: $

(in-package :gsl)

(defmfun clausen (x)
  "gsl_sf_clausen_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Clausen integral Cl_2(x).")

#|
(make-tests clausen
	    (clausen 2.5d0))
|#

(LISP-UNIT:DEFINE-TEST CLAUSEN
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4335982032355329d0 1.2032502825912828d-15)
   (MULTIPLE-VALUE-LIST (CLAUSEN 2.5d0))))
