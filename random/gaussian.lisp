;; Gaussian distribution
;; Liam Healy, Sun Jul 16 2006 - 22:09
;; Time-stamp: <2008-02-17 12:23:52EST gaussian.lisp>
;; $Id: $

(in-package :gsl)

(defmfun gaussian (generator sigma)
  "gsl_ran_gaussian"
  (((generator generator) :pointer) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "A Gaussian random variate, with mean zero and
  standard deviation sigma.  The probability distribution for
  Gaussian random variates is
  p(x) dx = {1 \over \sqrt{2 \pi \sigma^2}} \exp (-x^2 / 2\sigma^2) dx
  for x in the range -\infty to +\infty.  Use the
  transformation z = \mu + x on the numbers returned by
  #'gaussian to obtain a Gaussian distribution with mean
  mu.  This function uses the Box-Mueller algorithm which requires two
  calls to the random number generator r.")

(defmfun gaussian-pdf (x sigma)
  "gsl_ran_gaussian_pdf" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "Compute the probability density p(x) at x
   for a Gaussian distribution with standard deviation sigma.")

(defmfun gaussian-ziggurat (generator sigma)
  "gsl_ran_gaussian_ziggurat"
  (((generator generator) :pointer) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "Compute a Gaussian random variate using the alternative
   Marsaglia-Tsang ziggurat method. The Ziggurat algorithm
   is the fastest available algorithm in most cases.")

(defmfun gaussian-ratio-method (generator sigma)
  "gsl_ran_gaussian_ratio_method"
  (((generator generator) :pointer) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "Compute a Gaussian random variate using the Kinderman-Monahan-Leva
   ratio method.")

(defmfun ugaussian (generator)
  "gsl_ran_ugaussian" (((generator generator) :pointer))
  :c-return :double
  :documentation			; FDL
  "Compute results for the unit Gaussian distribution,
   equivalent to the #'gaussian with a standard deviation of one,
   sigma = 1.")

(defmfun ugaussian-pdf (x)
  "gsl_ran_ugaussian_pdf" ((x :double))
  :c-return :double
  :documentation			; FDL
  "Compute results for the unit Gaussian distribution,
   equivalent to the #'gaussian-pdf with a standard deviation of one,
   sigma = 1.")

(defmfun ugaussian-ratio-method (generator)
  "gsl_ran_ugaussian_ratio_method"
  (((generator generator) :pointer))
  :c-return :double
  :documentation			; FDL
  "Compute results for the unit Gaussian distribution,
   equivalent to the #'gaussian-ration-method with a
   standard deviation of one, sigma = 1.")

(defmfun gaussian-P (x sigma)
  "gsl_cdf_gaussian_P" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function P(x) for the Gaussian
   distribution with standard deviation sigma.")

(defmfun gaussian-Q (x sigma)
  "gsl_cdf_gaussian_Q" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function Q(x) for the Gaussian
   distribution with standard deviation sigma.")

(defmfun gaussian-Pinv (P sigma)
  "gsl_cdf_gaussian_Pinv" ((P :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function P(x) for the Gaussian
   distribution with standard deviation sigma.")

(defmfun gaussian-Qinv (Q sigma)
  "gsl_cdf_gaussian_Qinv" ((Q :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function Q(x) for the Gaussian
   distribution with standard deviation sigma.")

(defmfun ugaussian-P (x)
  "gsl_cdf_ugaussian_P" ((x :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function P(x) for the Gaussian
   distribution with unit standard deviation.")

(defmfun ugaussian-Q (x)
  "gsl_cdf_ugaussian_Q" ((x :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function Q(x) for the Gaussian
   distribution with unit standard deviation.")

(defmfun ugaussian-Pinv (P)
  "gsl_cdf_ugaussian_Pinv" ((P :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function P(x) for the Gaussian
   distribution with unit standard deviation.")

(defmfun ugaussian-Qinv (Q)
  "gsl_cdf_ugaussian_Qinv" ((Q :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function Q(x) for the Gaussian
   distribution with unit standard deviation.")

;;; Examples and unit test
#|
(make-tests
 gaussian
 (letm ((rng (random-number-generator *mt19937* 0)))
   (loop for i from 0 to 10
	 collect
	 (gaussian rng 10.0d0)))
 (gaussian-pdf 0.0d0 10.0d0)
 (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (gaussian-ziggurat rng 10.0d0)))
 ;; Given in examples in GSL documentation
 (ugaussian-p 2.0d0)
 (ugaussian-q 2.0d0)
 (ugaussian-pinv 0.9772498680518208d0)
 (ugaussian-qinv 0.02275013194817921d0))
|#

(LISP-UNIT:DEFINE-TEST GAUSSIAN
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 1.3391860811867589d0 -0.8810099183143839d0
	  16.744084062537738d0 7.336411072925795d0
	  9.975246316020124d0 -12.775020810027664d0
	  -23.967152827332075d0 -6.79280164729211d0
	  -0.3909131843358723d0 8.935555455208181d0
	  -0.17647794589783283d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (GAUSSIAN RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.039894228040143274d0)
   (MULTIPLE-VALUE-LIST (GAUSSIAN-PDF 0.0d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 6.107926637756835d0 -15.764917894916914d0
	  -7.200971392533084d0 19.5115024682162d0
	  5.248202921580308d0 10.408510262302668d0
	  -14.333242326151595d0 0.9455088719230575d0
	  -11.120343985926329d0 -3.0791957427954d0
	  -2.090680773068803d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (GAUSSIAN-ZIGGURAT RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9772498680518208d0)
   (MULTIPLE-VALUE-LIST (UGAUSSIAN-P 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.022750131948179212d0)
   (MULTIPLE-VALUE-LIST (UGAUSSIAN-Q 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.0d0)
   (MULTIPLE-VALUE-LIST (UGAUSSIAN-PINV 0.9772498680518208d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.0d0)
   (MULTIPLE-VALUE-LIST (UGAUSSIAN-QINV 0.02275013194817921d0))))
