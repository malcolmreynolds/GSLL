;********************************************************
; file:        debye.lisp                                
; description: Deybe functions                           
; date:        Sun Mar 19 2006 - 14:34                   
; author:      Liam M. Healy                             
; modified:    Sun Mar 19 2006 - 14:37
;********************************************************
;;; $Id: $

#|
The Debye functions @math{D_n(x)} are defined by the following integral,
@tex
\beforedisplay
$$
D_n(x) = {n \over x^n} \int_0^x dt {t^n \over e^t - 1}
$$
\afterdisplay
@end tex
@ifinfo

@example
D_n(x) = n/x^n \int_0^x dt (t^n/(e^t - 1))
@end example
@end ifinfo
@noindent
For further information see Abramowitz &
Stegun, Section 27.1.
|#

(in-package :gsl)

(defun-sf debye-1  ((x :double))
  "gsl_sf_debye_1_e"
  :documentation
  "The first-order Debye function @math{D_1(x) = (1/x) \int_0^x dt (t/(e^t - 1))}."
  :return (sf-result))

(defun-sf debye-2  ((x :double))
  "gsl_sf_debye_2_e"
  :documentation
  "The second-order Debye function @math{D_2(x) = (2/x^2) \int_0^x dt (t^2/(e^t - 1))}."
  :return (sf-result))

(defun-sf debye-3  ((x :double))
  "gsl_sf_debye_3_e"
  :documentation
  "The third-order Debye function @math{D_3(x) = (3/x^3) \int_0^x dt (t^3/(e^t - 1))}."
  :return (sf-result))

(defun-sf debye-4  ((x :double))
  "gsl_sf_debye_4_e"
  :documentation
  "The fourth-order Debye function @math{D_4(x) = (4/x^4) \int_0^x dt (t^4/(e^t - 1))}."
  :return (sf-result))
