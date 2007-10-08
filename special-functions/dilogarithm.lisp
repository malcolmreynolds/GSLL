;********************************************************
; file:        dilogarithm.lisp                          
; description: Dilogarithm                               
; date:        Fri Mar 17 2006 - 18:44                   
; author:      Liam M. Healy
; modified:    Mon Oct  8 2007 - 11:27
;********************************************************

(in-package :gsl)

;;; dilog merge complex and real
(defgeneric dilogarithm (x)
  (:documentation "The dilogarithm."))

(defun-gsl dilogarithm ((x float))
  "gsl_sf_dilog_e" ((x :double) (ret sf-result))
  :type :method)

(defun-gsl dilogarithm ((x complex))
  "gsl_sf_complex_dilog_e"
  (((abs x) :double) ((phase x) :double) (re sf-result) (im sf-result))
  :type :method
  :return ((complex (val re) (val im)) (complex (err re) (err im))))

(lisp-unit:define-test dilogarithm
  (lisp-unit:assert-first-fp-equal
   "0.164493406685d+01" (dilogarithm 1.0d0))
  (lisp-unit:assert-equal
   '("-0.205616758356d+00" "0.915965594177d+00")
   (let ((c (dilogarithm #c(0.0d0 1.0d0))))
     (lisp-unit:fp-sequence (list (realpart c) (imagpart c))))))
