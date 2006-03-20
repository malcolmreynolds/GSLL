;********************************************************
; file:        dawson.lisp                               
; description: Dawson function                           
; date:        Sun Mar 19 2006 - 14:31                   
; author:      Liam M. Healy                             
; modified:    Sun Mar 19 2006 - 14:33
;********************************************************
;;; $Id: $

#|
The Dawson integral is defined by @math{\exp(-x^2) \int_0^x dt
\exp(t^2)}.  A table of Dawson's integral can be found in Abramowitz &
Stegun, Table 7.5.  The Dawson functions are declared in the header file
@file{gsl_sf_dawson.h}.
|#

(in-package :gsl)

(defun-sf dawson ((x :double))
  "gsl_sf_dawson_e"
  :documentation
  "Dawson's integral for @var{x}."
  :return (sf-result))
