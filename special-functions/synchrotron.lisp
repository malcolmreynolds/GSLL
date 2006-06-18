;********************************************************
; file:        synchrotron.lisp                                
; description: Synchrotron functions
; date:        Mon May  1 2006 - 22:29
; author:      Liam M. Healy                             
; modified:    Sat Jun 17 2006 - 22:32
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl synchrotron-1 (x)
  "gsl_sf_synchrotron_1_e" ((x :double) (ret sf-result))
  :documentation "The first synchrotron function 
  @math{x \int_x^\infty dt K_@{5/3@}(t)} for @math{x >= 0}.")

(defun-gsl synchrotron-2 (x)
  "gsl_sf_synchrotron_2_e" ((x :double) (ret sf-result))
  :documentation "The second synchrotron function 
  @math{x K_@{2/3@}(x)} for @math{x >= 0}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test synchrotron
  (lisp-unit:assert-first-fp-equal "0.528273966979d-01" (synchrotron-1 4.0d0))
  (lisp-unit:assert-first-fp-equal "0.469232058261d-01" (synchrotron-2 4.0d0)))
