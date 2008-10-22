;; Clausen function
;; Liam Healy, Sat Mar 18 2006 - 23:18
;; Time-stamp: <2008-10-20 22:26:30EDT clausen.lisp>
;; $Id$

(in-package :gsl)

(defmfun clausen (x)
  "gsl_sf_clausen_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Clausen integral Cl_2(x).")

(save-test clausen (clausen 2.5d0))
