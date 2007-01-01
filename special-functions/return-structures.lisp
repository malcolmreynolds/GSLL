;********************************************************
; file:        return-structures.lisp                    
; description: Structures returned by special functions. 
; date:        Mon Jan  1 2007 - 11:35                   
; author:      Liam M. Healy                             
; modified:    Mon Jan  1 2007 - 11:35
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Result from special functions
;;;;****************************************************************************

(cffi:defcstruct sf-result
  "Results from special functions with value and error estimate."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
  (val :double)
  (err :double))

(cffi:defcstruct sf-result-e10
  "Results from special functions with value, error estimate
and a scaling exponent e10, such that the value is val*10^e10."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
  (val :double)
  (err :double)
  (e10 :int))

(cffi:defcenum sf-mode
  "Numerical precision modes with which to calculate special functions."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC62
  :double-prec
  :single-prec
  :approx-prec)

(defun val (sf-result &optional (type 'sf-result))
  (cffi:foreign-slot-value sf-result type 'val))

(defun err (sf-result &optional (type 'sf-result))
  (cffi:foreign-slot-value sf-result type 'err))

(defun e10 (sf-result)
  (cffi:foreign-slot-value sf-result 'sf-result-e10 'e10))

;;; obsolete; just do this manually for the few cases of multiple sf-result returns?
(defun rearrange-sf-result-err (return-list)
  "Put the 'err values from the sf-results at the end of the return values."
  (flet ((sf-err (x)
	   (and (eq (first x) 'cffi:foreign-slot-value)
		(member (third x) '('sf-result 'sf-result-e10) :test #'equal)
		(equal (fourth x) ''err))))
    (append
     (remove-if #'sf-err return-list)
     (remove-if-not #'sf-err return-list))))
