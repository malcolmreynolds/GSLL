;********************************************************
; file:        coupling.lisp                             
; description: Coupling coefficients                     
; date:        Sun Mar 19 2006 - 13:30                   
; author:      Liam M. Healy                             
; modified:    Sun Mar 19 2006 - 14:25
;********************************************************
;;; $Id: $

#|
The Wigner 3-j, 6-j and 9-j symbols give the coupling coefficients for
combined angular momentum vectors.  Since the arguments of the standard
coupling coefficient functions are integer or half-integer, the
arguments of the following functions are, by convention, integers equal
to twice the actual spin value.  For information on the 3-j coefficients
see Abramowitz & Stegun, Section 27.9.  The functions described in this
section are declared in the header file @file{gsl_sf_coupling.h}.
|#

(in-package :gsl)

(defun-sf coupling-3j ((two-ja :int) (two-jb :int) (two-jc :int)
		       (two-ma :int) (two-mb :int) (two-mc :int)) 
  "gsl_sf_coupling_3j_e"
  :documentation
  "The Wigner 3-j coefficient, 
@tex
\beforedisplay
$$
\pmatrix{ja & jb & jc\cr
         ma & mb & mc\cr}
$$
\afterdisplay
@end tex
where the arguments are given in half-integer units, @math{ja} =
@var{two_ja}/2, @math{ma} = @var{two_ma}/2, etc."
  :return (sf-result))

(defun-sf coupling-6j ((two-ja :int) (two-jb :int) (two-jc :int)
		       (two-jd :int) (two-je :int) (two-jf :int)) 
  "gsl_sf_coupling_6j_e"
  :documentation
  "The Wigner 6-j coefficient, 
@tex
\beforedisplay
$$
\left\{\matrix{ja & jb & jc\cr
               jd & je & jf\cr}\right\}
$$
\afterdisplay
@end tex
where the arguments are given in half-integer units, @math{ja} =
@var{two_ja}/2, @math{ma} = @var{two_ma}/2, etc."
  :return (sf-result))

(defun-sf coupling-9j ((two-ja :int) (two-jb :int) (two-jc :int)
		       (two-jd :int) (two-je :int) (two-jf :int)
		       (two-jg :int) (two-jh :int) (two-ji :int)) 
  "gsl_sf_coupling_9j_e"
  :documentation
"The Wigner 9-j coefficient, 
@tex
\beforedisplay
$$
\left\{\matrix{ja & jb & jc\cr
               jd & je & jf\cr
               jg & jh & ji\cr}\right\}
$$
\afterdisplay
@end tex
where the arguments are given in half-integer units, @math{ja} =
@var{two_ja}/2, @math{ma} = @var{two_ma}/2, etc."
  :return (sf-result))


