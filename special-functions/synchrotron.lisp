;********************************************************
; file:        synchrotron.lisp                                
; description: Synchrotron functions
; date:        Mon May  1 2006 - 22:29
; author:      Liam M. Healy                             
; modified:    Mon May  1 2006 - 22:30
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl synchrotron-1 ((x :double))
  "gsl_sf_synchrotron_1_e"
  :documentation "The first synchrotron function 
  @math{x \int_x^\infty dt K_@{5/3@}(t)} for @math{x >= 0}."
  :return (sf-result))

(defun-gsl synchrotron-2 ((x :double))
  "gsl_sf_synchrotron_2_e"
  :documentation "The second synchrotron function 
  @math{x K_@{2/3@}(x)} for @math{x >= 0}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test synchrotron
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.528273966979d-01" (SYNCHROTRON-1 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.469232058261d-01" (SYNCHROTRON-2 4.0d0)))
