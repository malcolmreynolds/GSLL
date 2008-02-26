;; Dilogarithm
;; Liam Healy, Fri Mar 17 2006 - 18:44
;; Time-stamp: <2008-02-16 20:19:55EST dilogarithm.lisp>
;; $Id$

(in-package :gsl)

;;; dilog merge complex and real
(defgeneric dilogarithm (x)
  (:documentation			; FDL
   "The dilogarithm."))

(defmfun dilogarithm ((x float))
  "gsl_sf_dilog_e" ((x :double) (ret sf-result))
  :type :method)

(defmfun dilogarithm ((x complex))
  "gsl_sf_complex_dilog_e"
  (((abs x) :double) ((phase x) :double) (re sf-result) (im sf-result))
  :type :method
  :return ((complex (val re) (val im)) (complex (err re) (err im))))

#|
(make-tests dilogarithm
	    (dilogarithm 1.0d0)
	    (dilogarithm #c(0.0d0 1.0d0)))
|#

(LISP-UNIT:DEFINE-TEST DILOGARITHM
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.6449340668482264d0 7.304974700020789d-16)
   (MULTIPLE-VALUE-LIST (DILOGARITHM 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #C(-0.20561675835602822d0 0.915965594177219d0)
	 #C(2.100180226255977d-15 7.618282373747058d-16))
   (MULTIPLE-VALUE-LIST (DILOGARITHM #C(0.0d0 1.0d0)))))

