;********************************************************
; file:        dilogarithm.lisp                          
; description: Dilogarithm                               
; date:        Fri Mar 17 2006 - 18:44                   
; author:      Liam M. Healy
; modified:    Wed May 24 2006 - 22:55
;********************************************************

(in-package :gsl)

;;; dilog merge complex and real
(defgeneric dilogarithm (x)
  (:documentation "The dilogarithm."))

(defun-gsl dilogarithm ((x :double))
  "gsl_sf_dilog_e"
  :method ((x double-float))
  :return (sf-result))

(defun-gsl dilogarithm (((abs x) :double) ((phase x) :double))
  "gsl_sf_complex_dilog_e"
  :method ((x complex))
  :return (sf-result sf-result)
  :multiple-returns
  (lambda (re im re-err im-err)
    `(values (complex ,re ,im) (complex ,re-err ,im-err))))

(lisp-unit:define-test dilogarithm
  (lisp-unit:assert-first-fp-equal "0.164493406685d+01" (dilogarithm 1.0d0)))
