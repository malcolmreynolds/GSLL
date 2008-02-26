;; Elementary functions
;; Liam Healy, Mon Mar 20 2006 - 21:43
;; Time-stamp: <2008-02-16 20:22:17EST elementary.lisp>
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

#|
(make-tests elementary
	    (multiply 3.0d0 2.0d0)
	    (multiply-err 3.0d0 0.1d0 2.0d0 0.1d0))
|#

(LISP-UNIT:DEFINE-TEST ELEMENTARY
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 6.0d0 2.6645352591003757d-15)
   (MULTIPLE-VALUE-LIST (MULTIPLY 3.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 6.0d0 0.5000000000000027d0)
   (MULTIPLE-VALUE-LIST
    (MULTIPLY-ERR 3.0d0 0.1d0 2.0d0 0.1d0))))
