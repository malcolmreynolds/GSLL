;; Logarithm
;; Liam Healy, Sun Apr 30 2006 - 22:08
;; Time-stamp: <2008-02-16 22:34:59EST logarithm.lisp>
;; $Id: $

(in-package :gsl)

(defgeneric gsl-log (x)
  (:documentation			; FDL
   "The natural logarithm of x, log(x), for x > 0."))

(defmfun gsl-log ((x float))
  "gsl_sf_log_e"
  ((x :double) (ret sf-result))
  :type :method
  :export t)

(defmfun gsl-log ((x complex))
  "gsl_sf_complex_log_e"
  (((realpart x) :double) ((imagpart x) :double)
   (re-ret sf-result) (im-ret sf-result))
  :type :method
  :return
  ((complex (val re-ret) (val im-ret)) (complex (err re-ret) (err im-ret)))
  :documentation			; FDL
  "Results are returned as lnr, theta such that
  exp(lnr + i \theta) = z_r + i z_i, where theta lies in the range [-\pi,\pi].")

(defmfun log-abs (x)
  "gsl_sf_log_abs_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The natural logarithm of the magnitude of x, log(|x|), for x ne 0.")

(defmfun log-1+x (x)
  "gsl_sf_log_1plusx_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "log(1 + x) for x > -1 using an algorithm that is accurate for small x.")

(defmfun log-1+x-m1 (x)
  "gsl_sf_log_1plusx_mx_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "log(1 + x) - x for x > -1 using an algorithm that is accurate for small x.")

;;; Examples and unit test

#|
(make-tests logarithm
  (gsl-log 2.0d0)
  (gsl-log #C(1.0d0 1.0d0))
  (log-abs -2.0d0)
  (log-1+x 1.d-4)
  (log-1+x-m1 1.d-4))
|#

(LISP-UNIT:DEFINE-TEST LOGARITHM
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6931471805599453d0 3.078191837246648d-16)
   (MULTIPLE-VALUE-LIST (GSL-LOG 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #C(0.34657359027997264d0 0.7853981633974483d0)
	 #C(1.539095918623324d-16 7.69547959311662d-17))
   (MULTIPLE-VALUE-LIST (GSL-LOG #C(1.0d0 1.0d0))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6931471805599453d0 3.078191837246648d-16)
   (MULTIPLE-VALUE-LIST (LOG-ABS -2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 9.999500033330834d-5 2.2203350343487824d-20)
   (MULTIPLE-VALUE-LIST (LOG-1+X 1.d-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -4.999666691664667d-9 1.1101490153075193d-24)
   (MULTIPLE-VALUE-LIST (LOG-1+X-M1 1.d-4))))
