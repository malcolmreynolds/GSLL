;; Exponential functions
;; Liam Healy, Tue Mar 21 2006 - 17:05
;; Time-stamp: <2008-02-16 21:01:05EST exponential-functions.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Exponential Functions
;;;;****************************************************************************

(defmfun gsl-exp (x)
  "gsl_sf_exp_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The exponential function.")

(defmfun exp-scaled (x)
  "gsl_sf_exp_e10_e" ((x :double) (ret sf-result-e10))
  :documentation			; FDL
  "The exponential function scaled. This function may be useful if the value
   of exp(x) would overflow the numeric range of double.")

(defmfun exp-mult (x y)
  "gsl_sf_exp_mult_e" ((x :double) (y :double) (ret sf-result))
  :documentation			; FDL
  "Exponentiate x and multiply by the factor y to
   return the product y \exp(x).")

(defmfun exp-mult-scaled (x y)
  "gsl_sf_exp_mult_e10_e" ((x :double) (y :double) (ret sf-result-e10))
  :documentation			; FDL
  "The product y \exp(x) with extended numeric range.")

;;;;****************************************************************************
;;;; Relative Exponential Functions
;;;;****************************************************************************

(defmfun expm1 (x)
  "gsl_sf_expm1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "\exp(x)-1 using an algorithm that is accurate for small x.")

(defmfun exprel (x)
  "gsl_sf_exprel_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "(\exp(x)-1)/x using an algorithm that is accurate for small x.
  For small x the algorithm is based on the expansion
  (\exp(x)-1)/x = 1 + x/2 + x^2/(2*3) + x^3/(2*3*4) + ...")

(defmfun exprel-2 (x)
  "gsl_sf_exprel_2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "2(\exp(x)-1-x)/x^2 using an algorithm that is accurate for small
   x.  For small x the algorithm is based on the expansion
   2(\exp(x)-1-x)/x^2 = 1 + x/3 + x^2/(3*4) + x^3/(3*4*5) + ...")

(defmfun exprel-n (n x)
  "gsl_sf_exprel_n_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "N-relative exponential, which is the n-th generalization
   of the functions #'exprel and #'exprel-2.")

;;;;****************************************************************************
;;;; Exponentiation With Error Estimate
;;;;****************************************************************************

(defmfun exp-err (x dx)
  "gsl_sf_exp_err_e" ((x :double) (dx :double) (ret sf-result))
  :documentation			; FDL
  "Exponentiate x with an associated absolute error dx.")

(defmfun exp-err-scaled (x dx)
  "gsl_sf_exp_err_e10_e"
  ((x :double) (dx :double) (ret sf-result))
  :documentation			; FDL
  "Exponentiate x with an associated absolute error dx
  and with extended numeric range.")

(defmfun exp-mult-err (x dx y dy)
  "gsl_sf_exp_mult_err_e"
  ((x :double) (dx :double) (y :double) (dy :double) (ret sf-result))
  :documentation			; FDL
  "The product y \exp(x) for the quantities x, y
   with associated absolute errors dx, dy.")

(defmfun exp-mult-err-scaled (x y)
  "gsl_sf_exp_mult_err_e10_e" ((x :double) (y :double) (ret sf-result-e10))
  :documentation			; FDL
  "The product y \exp(x) for the quantities x, y
   with associated absolute errors dx, dy and with
   extended numeric range.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests exponential-functions
  (gsl-exp 3.0d0)
  (exp-scaled 555.0d0)
  (exp-mult 101.0d0 5.0d0)
  (exp-mult-scaled 555.0d0 101.0d0)
  (expm1 0.0001d0)
  (exprel 0.0001d0)
  (exprel-2 0.001d0)
  (exprel-n 3 0.001d0)
  (exp-err 3.0d0 0.001d0)
  (exp-mult-err 3.0d0 0.001d0 23.0d0 0.001d0))
|#

(LISP-UNIT:DEFINE-TEST EXPONENTIAL-FUNCTIONS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 20.085536923187668d0 8.91977022163267d-15)
   (MULTIPLE-VALUE-LIST (GSL-EXP 3.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0800340716201098d0 241 2.666751014771678d-13)
   (MULTIPLE-VALUE-LIST (EXP-SCALED 555.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3.6535299896840335d44 8.355840218353793d30)
   (MULTIPLE-VALUE-LIST (EXP-MULT 101.0d0 5.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0908344123363103d0 243 2.7201204352005766d-15)
   (MULTIPLE-VALUE-LIST
    (EXP-MULT-SCALED 555.0d0 101.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0000500016667085d-4 4.441114150507224d-20)
   (MULTIPLE-VALUE-LIST (EXPM1 1.d-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0000500016667084d0 4.4411141505072235d-16)
   (MULTIPLE-VALUE-LIST (EXPREL 1.d-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0003334166833362d0 4.442372766015162d-16)
   (MULTIPLE-VALUE-LIST (EXPREL-2 0.001d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0002500500083344d0 2.665201526164121d-15)
   (MULTIPLE-VALUE-LIST (EXPREL-N 3 0.001d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 20.085536923187668d0 0.04017108054156605d0)
   (MULTIPLE-VALUE-LIST (EXP-ERR 3.0d0 0.001d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 461.9673492333164d0 0.4820528861567092d0)
   (MULTIPLE-VALUE-LIST
    (EXP-MULT-ERR 3.0d0 0.001d0 23.0d0 0.001d0))))

