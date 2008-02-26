;; Gamma distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 13:10:14EST gamma.lisp>
;; $Id$

(in-package :gsl)

(defmfun gamma-rd (generator a b)
  ;; Named #'gamma-rd to avoid confusion with the special function #'gamma.
  "gsl_ran_gamma"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the gamma distribution.
   The distribution function is
   p(x) dx = {1 \over \Gamma(a) b^a} x^{a-1} e^{-x/b} dx
   for x > 0. The gamma distribution with an integer parameter a
   is known as the Erlang distribution.  The variates are computed using
   the algorithms from Knuth (vol 2).")

(defmfun gamma-mt (generator a b)
  "gsl_ran_gamma_mt"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A gamma variate using the Marsaglia-Tsang fast gamma method.")

(defmfun gamma-pdf (x a b)
  "gsl_ran_gamma_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a gamma distribution with parameters a and b, using the
   formula given in #'gamma.")

(defmfun gamma-P (x a b)
  "gsl_cdf_gamma_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the Gamma distribution with parameters a and b.")

(defmfun gamma-Q (x a b)
  "gsl_cdf_gamma_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the Gamma distribution with parameters a and b.")

(defmfun gamma-Pinv (P a b)
  "gsl_cdf_gamma_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the Gamma distribution with parameters a and b.")

(defmfun gamma-Qinv (Q a b)
  "gsl_cdf_gamma_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the Gamma distribution with parameters a and b.")

;;; Examples and unit test
#|
(make-tests gamma-randist
 (letm ((rng (random-number-generator *mt19937* 0)))
   (loop for i from 0 to 10
	 collect
	 (gamma-rd rng 1.0d0 2.0d0)))
 (letm ((rng (random-number-generator *mt19937* 0)))
   (loop for i from 0 to 10
	 collect
	 (gamma-mt rng 1.0d0 2.0d0)))
 (gamma-pdf 0.1d0 1.0d0 2.0d0)
 (gamma-P 0.1d0 1.0d0 2.0d0)
 (gamma-Q 0.1d0 1.0d0 2.0d0)
 (gamma-Pinv 0.048770575499286005d0 1.0d0 2.0d0)
 (gamma-Qinv 0.951229424500714d0 1.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST GAMMA-RANDIST
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 5.165688917678959d-4 3.6291162855975294d0
	  2.5273196108679516d0 0.10848774504124709d0
	  2.9249988468316213d0 1.4473215859071986d0
	  0.08690724899367205d0 0.590607841809058d0
	  1.2322105878131593d0 0.6023372667078228d0
	  0.5490215963871072d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (GAMMA-RD RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 2.6001378761310767d0 2.5222666954201554d0
	  7.731422092125877d0 4.227279926486253d0
	  0.09519304347489156d0 0.5710920106874436d0
	  0.8910637719460512d0 0.8263221202545431d0
	  3.1830665720573026d0 0.0038084003613199297d0
	  1.0320117334083205d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (GAMMA-MT RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.475614712250357d0)
   (MULTIPLE-VALUE-LIST (GAMMA-PDF 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.04877057549928599d0)
   (MULTIPLE-VALUE-LIST (GAMMA-P 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.951229424500714d0)
   (MULTIPLE-VALUE-LIST (GAMMA-Q 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.10000000000000006d0)
   (MULTIPLE-VALUE-LIST
    (GAMMA-PINV 0.048770575499286005d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.10000000000000003d0)
   (MULTIPLE-VALUE-LIST
    (GAMMA-QINV 0.951229424500714d0 1.0d0 2.0d0))))

