;; Coupling coefficients
;; Liam Healy, Sun Mar 19 2006 - 13:30
;; Time-stamp: <2008-10-23 22:39:07EDT coupling.lisp>
;; $Id$

(in-package :gsl)

#|
;;; FDL
The Wigner 3-j, 6-j and 9-j symbols give the coupling coefficients for
combined angular momentum vectors.  Since the arguments of the standard
coupling coefficient functions are integer or half-integer, the
arguments of the following functions are, by convention, integers equal
to twice the actual spin value.  For information on the 3-j coefficients
see Abramowitz & Stegun, Section 27.9.  The functions described in this
section are declared in the header file gsl_sf_coupling.h.
|#

(defmfun coupling-3j (two-ja two-jb two-jc two-ma two-mb two-mc)
  "gsl_sf_coupling_3j_e" 
  ((two-ja :int) (two-jb :int) (two-jc :int)
   (two-ma :int) (two-mb :int) (two-mc :int)
   (ret sf-result))
  :documentation			; FDL
  "The Wigner 3-j coefficient, 
  \pmatrix{ja & jb & jc\cr
         ma & mb & mc\cr}
  where the arguments are given in half-integer units,
  ja = two_ja/2, ma = two_ma/2, etc.")

(defmfun coupling-6j (two-ja two-jb two-jc two-jd two-je two-jf)
  "gsl_sf_coupling_6j_e" 
  ((two-ja :int) (two-jb :int) (two-jc :int)
   (two-jd :int) (two-je :int) (two-jf :int)
   (ret sf-result))
  :documentation			; FDL
  "The Wigner 6-j coefficient, 
   ja & jb & jc
   jd & je & jf
  where the arguments are given in half-integer units, ja =
  two_ja/2, ma = two_ma/2, etc.")

(defmfun coupling-9j
    (two-ja two-jb two-jc two-jd two-je two-jf two-jg two-jh two-ji)
  "gsl_sf_coupling_9j_e" 
  ((two-ja :int) (two-jb :int) (two-jc :int)
   (two-jd :int) (two-je :int) (two-jf :int)
   (two-jg :int) (two-jh :int) (two-ji :int)
   (ret sf-result))
  :documentation			; FDL
  "The Wigner 9-j coefficient, 
  ja & jb & jc
  jd & je & jf
  jg & jh & ji
  where the arguments are given in half-integer units,
  ja = two_ja/2, ma = two_ma/2, etc.")

;; Check with online calculator http://www-stone.ch.cam.ac.uk/wigner.html
(save-test coupling
  (coupling-3j 0 1 1 0 1 -1)
  (coupling-6j 1 1 2 0 2 1)
  (coupling-9j 1 1 2 1 2 1 2 1 1))
