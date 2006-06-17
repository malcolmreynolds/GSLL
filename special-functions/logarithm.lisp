;********************************************************
; file:        logarithm.lisp                            
; description: Logarithm                                 
; date:        Sun Apr 30 2006 - 22:08                   
; author:      Liam M. Healy                             
; modified:    Fri Jun 16 2006 - 22:20
;********************************************************
;;; $Id: $

(in-package :gsl)

(defgeneric gsl-log (x)
  (:documentation
   "The natural logarithm of @var{x}, @math{\log(x)}, for @math{x > 0}."))

(defun-gsl gsl-log ((x double-float))
  "gsl_sf_log_e" ((x :double) (ret sf-result))
  :type :method
  :export t)

(defun-gsl gsl-log ((x complex))
  "gsl_sf_complex_log_e"
  (((realpart x) :double) ((imagpart x) :double)
   (re-ret sf-result) (im-ret sf-result))
  :type :method
  :documentation "Results are returned as @var{lnr}, @var{theta} such that
  @math{\exp(lnr + i \theta) = z_r + i z_i}, where @math{\theta} lies in
  the range @math{[-\pi,\pi]}."
  :return
  ((complex (val re-ret) (val im-ret)) (complex (err re-ret) (err im-ret))))

(defun-gsl log-abs (x)
  "gsl_sf_log_abs_e" ((x :double) (ret sf-result))
  :documentation
  "The natural logarithm of the magnitude of @var{x},
  @math{\log(|x|)}, for @math{x \ne 0}.")

(defun-gsl log-1+x (x)
  "gsl_sf_log_1plusx_e" ((x :double) (ret sf-result))
  :documentation
  "@math{\log(1 + x)} for @math{x > -1} using an
   algorithm that is accurate for small @math{x}.")

(defun-gsl log-1+x-m1 (x)
  "gsl_sf_log_1plusx_mx_e" ((x :double) (ret sf-result))
  :documentation
  "@math{\log(1 + x) - x} for @math{x > -1} using an
  algorithm that is accurate for small @math{x}.")

(lisp-unit:define-test logarithm
  (lisp-unit:assert-first-fp-equal
   "0.693147180560d+00"
   (gsl-log 2.0d0))
  (lisp-unit:assert-equal
   '("0.346573590280d+00" "0.785398163397d+00")
   (lisp-unit::fp-string (gsl-log #C(1.0d0 1.0d0))))
  (lisp-unit:assert-first-fp-equal
   "0.693147180560d+00"
   (log-abs -2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.999950003333d-04"
   (log-1+x 1.d-4))
  (lisp-unit:assert-first-fp-equal
   "-0.499966669166d-08"
   (log-1+x-m1 1.d-4)))
