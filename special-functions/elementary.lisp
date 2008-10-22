;; Elementary functions
;; Liam Healy, Mon Mar 20 2006 - 21:43
;; Time-stamp: <2008-10-21 22:58:45EDT elementary.lisp>
;; $Id$

(in-package :gsl)

(defmfun multiply (x y)
  "gsl_sf_multiply_e"
  ((x :double) (y :double) (ret sf-result))
  :documentation			; FDL
  "Multiplies x and y returning the product and associated error.")

(defmfun multiply-err (x dx y dy)
    "gsl_sf_multiply_err_e"
  ((x :double) (dx :double) (y :double)  (dy :double) (ret sf-result))
  :documentation			; FDL
  "Multiplies x and y with associated absolute
   errors dx and dy.  The product xy +/- xy \sqrt((dx/x)^2 +(dy/y)^2)
   is returned.")

(save-test elementary
	   (multiply 3.0d0 2.0d0)
	   (multiply-err 3.0d0 0.1d0 2.0d0 0.1d0))

