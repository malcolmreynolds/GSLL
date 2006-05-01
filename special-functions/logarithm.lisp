;********************************************************
; file:        logarithm.lisp                            
; description: Logarithm                                 
; date:        Sun Apr 30 2006 - 22:08                   
; author:      Liam M. Healy                             
; modified:    Sun Apr 30 2006 - 22:43
;********************************************************
;;; $Id: $

(in-package :gsl)

(defgeneric gsl-log (x)
  (:documentation
   "The natural logarithm of @var{x}, @math{\log(x)}, for @math{x > 0}."))

(defun-gsl gsl-log ((x :double))
  "gsl_sf_log_e"
  :method ((x double-float))
  :return (sf-result))

(defun-gsl gsl-log (((realpart x) :double) ((imagpart x) :double))
  "gsl_sf_complex_log_e"
  :method ((x complex))
  :documentation "Results are returned as @var{lnr}, @var{theta} such that
  @math{\exp(lnr + i \theta) = z_r + i z_i}, where @math{\theta} lies in
  the range @math{[-\pi,\pi]}."
  :return (sf-result sf-result))

(defun-gsl log-abs ((x :double))
  "gsl_sf_log_abs_e"
  :documentation
  "The natural logarithm of the magnitude of @var{x},
  @math{\log(|x|)}, for @math{x \ne 0}."
  :return (sf-result))

(defun-gsl log-1+x ((x :double))
  "gsl_sf_log_1plusx_e"
  :documentation
  "@math{\log(1 + x)} for @math{x > -1} using an
   algorithm that is accurate for small @math{x}."
  :return (sf-result))

(defun-gsl log-1+x-m1 ((x :double))
  "gsl_sf_log_1plusx_mx_e"
  :documentation
  "@math{\log(1 + x) - x} for @math{x > -1} using an
  algorithm that is accurate for small @math{x}."
  :return (sf-result))
