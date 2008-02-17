;; Jacobian elliptic functions
;; Liam Healy, Mon Mar 20 2006 - 22:21
;; Time-stamp: <2008-02-16 20:42:55EST elliptic-functions.lisp>
;; $Id: $

(in-package :gsl)

(defmfun jacobian-elliptic-functions (u m)
  "gsl_sf_elljac_e"
  ((u :double) (m :double) (sn sf-result) (cn sf-result) (dn sf-result))
  :return ((val sn) (val cn) (val dn) (err sn) (err cn) (err dn))
  :documentation			; FDL
  "The Jacobian elliptic functions sn(u|m),
  cn(u|m), dn(u|m) computed by descending Landen transformations.")

;;; > (jacobian-elliptic-functions 0.61802d0 0.5d0)
;;; 0.564575752943391
;;; 0.8253812568676386
;;; 0.916857191493965
;;; > (jacobian-elliptic-functions 0.2d0 0.81d0)
;;; 0.19762082367187697
;;; 0.9802785369736752
;;; 0.9840560289645665
;;; > (jacobian-elliptic-functions 0.61802d0 1.5d0)
;;; ;;;error


#|
(make-tests elliptic-functions
	    (jacobian-elliptic-functions 0.2d0 0.81d0)
	    (jacobian-elliptic-functions 0.61802d0 1.5d0))
|#

(LISP-UNIT:DEFINE-TEST ELLIPTIC-FUNCTIONS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.19762082367187703d0 0.9802785369736752d0
	 0.9840560289645665d0 0.0d0 0.0d0 0.0d0)
   (MULTIPLE-VALUE-LIST
    (JACOBIAN-ELLIPTIC-FUNCTIONS 0.2d0 0.81d0)))
  (LISP-UNIT:ASSERT-ERROR 'GSL-ERROR
			  (JACOBIAN-ELLIPTIC-FUNCTIONS
			   0.61802d0 1.5d0)))

