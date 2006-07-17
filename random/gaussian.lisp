;********************************************************
; file:        gaussian.lisp                             
; description: Gaussian distribution                     
; date:        Sun Jul 16 2006 - 22:09                   
; author:      Liam M. Healy                             
; modified:    Sun Jul 16 2006 - 22:42
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl gaussian (generator sigma)
  "gsl_ran_gaussian"
  (((generator generator) :pointer) (sigma :double))
  :c-return :double
  :documentation
  "A Gaussian random variate, with mean zero and
  standard deviation @var{sigma}.  The probability distribution for
  Gaussian random variates is
  p(x) dx = {1 \over \sqrt{2 \pi \sigma^2}} \exp (-x^2 / 2\sigma^2) dx
  for @math{x} in the range @math{-\infty} to @math{+\infty}.  Use the
  transformation @math{z = \mu + x} on the numbers returned by
  @code{gsl_ran_gaussian} to obtain a Gaussian distribution with mean
  @math{\mu}.  This function uses the Box-Mueller algorithm which requires two
  calls to the random number generator @var{r}.")

(defun-gsl gaussian-pdf (x sigma)
  "gsl_ran_gaussian_pdf" ((x :double) (sigma :double))
  :c-return :double
  :documentation
  "Compute the probability density @math{p(x)} at @var{x}
   for a Gaussian distribution with standard deviation @var{sigma}.")

(defun-gsl gaussian-ziggurat (generator sigma)
  "gsl_ran_gaussian_ziggurat"
  (((generator generator) :pointer) (sigma :double))
  :c-return :double
  :documentation
  "Compute a Gaussian random variate using the alternative
   Marsaglia-Tsang ziggurat method. The Ziggurat algorithm
   is the fastest available algorithm in most cases.")

(defun-gsl gaussian-ratio-method (generator sigma)
  "gsl_ran_gaussian_ratio_method"
  (((generator generator) :pointer) (sigma :double))
  :c-return :double
  :documentation
  "Compute a Gaussian random variate using the Kinderman-Monahan-Leva
   ratio method.")

(defun-gsl ugaussian (generator)
  "gsl_ran_ugaussian" (((generator generator) :pointer))
  :c-return :double
  :documentation
  "Compute results for the unit Gaussian distribution,
   equivalent to the #'gaussian with a standard deviation of one,
   @var{sigma} = 1.")

(defun-gsl ugaussian-pdf (x)
  "gsl_ran_ugaussian_pdf" ((x :double))
  :c-return :double
  :documentation
  "Compute results for the unit Gaussian distribution,
   equivalent to the #'gaussian-pdf with a standard deviation of one,
   @var{sigma} = 1.")

(defun-gsl ugaussian-ratio-method (generator)
  "gsl_ran_ugaussian_ratio_method"
  (((generator generator) :pointer))
  :c-return :double
  :documentation
  "Compute results for the unit Gaussian distribution,
   equivalent to the #'gaussian-ration-method with a
   standard deviation of one, @var{sigma} = 1.")

(defun-gsl gaussian-P (x sigma)
  "gsl_cdf_gaussian_P" ((x :double) (sigma :double))
  :c-return :double
  :documentation
  "The cumulative distribution function @math{P(x)} for the Gaussian
   distribution with standard deviation @var{sigma}.")

(defun-gsl gaussian-Q (x sigma)
  "gsl_cdf_gaussian_Q" ((x :double) (sigma :double))
  :c-return :double
  :documentation
  "The cumulative distribution function @math{Q(x)} for the Gaussian
   distribution with standard deviation @var{sigma}.")

(defun-gsl gaussian-Pinv (P sigma)
  "gsl_cdf_gaussian_Pinv" ((P :double) (sigma :double))
  :c-return :double
  :documentation
  "The inverse cumulative distribution function @math{P(x)} for the Gaussian
   distribution with standard deviation @var{sigma}.")

(defun-gsl gaussian-Qinv (Q sigma)
  "gsl_cdf_gaussian_Qinv" ((Q :double) (sigma :double))
  :c-return :double
  :documentation
  "The inverse cumulative distribution function @math{Q(x)} for the Gaussian
   distribution with standard deviation @var{sigma}.")

(defun-gsl ugaussian-P (x)
  "gsl_cdf_ugaussian_P" ((x :double))
  :c-return :double
  :documentation
  "The cumulative distribution function @math{P(x)} for the Gaussian
   distribution with unit standard deviation.")

(defun-gsl ugaussian-Q (x)
  "gsl_cdf_ugaussian_Q" ((x :double))
  :c-return :double
  :documentation
  "The cumulative distribution function @math{Q(x)} for the Gaussian
   distribution with unit standard deviation.")

(defun-gsl ugaussian-Pinv (P)
  "gsl_cdf_ugaussian_Pinv" ((P :double))
  :c-return :double
  :documentation
  "The inverse cumulative distribution function @math{P(x)} for the Gaussian
   distribution with unit standard deviation.")

(defun-gsl ugaussian-Qinv (Q)
  "gsl_cdf_ugaussian_Qinv" ((Q :double))
  :c-return :double
  :documentation
  "The inverse cumulative distribution function @math{Q(x)} for the Gaussian
   distribution with unit standard deviation.")

;;; Examples and unit test
(lisp-unit:define-test gaussian
  (lisp-unit:assert-equal
   '("0.133918608119d+01" "-0.881009918314d+00" "0.167440840625d+02"
     "0.733641107293d+01" "0.997524631602d+01" "-0.127750208100d+02"
     "-0.239671528273d+02" "-0.679280164729d+01" "-0.390913184336d+00"
     "0.893555545521d+01" "-0.176477945898d+00")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	 collect
	 (gaussian *rng-mt19937* 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.398942280401d-01"
   (gaussian-pdf 0.0d0 10.0d0))
  (lisp-unit:assert-equal
   '("0.610792663776d+01" "-0.157649178949d+02" "-0.720097139253d+01"
     "0.195115024682d+02" "0.524820292158d+01" "0.104085102623d+02"
     "-0.143332423262d+02" "0.945508871923d+00" "-0.111203439859d+02"
     "-0.307919574280d+01" "-0.209068077307d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	 collect
	 (gaussian-ziggurat *rng-mt19937* 10.0d0)))))
  ;; Given in examples in GSL documentation
  (lisp-unit:assert-first-fp-equal "0.977249868052d+00" (ugaussian-p 2.0d0))
  (lisp-unit:assert-first-fp-equal "0.227501319482d-01" (ugaussian-q 2.0d0))
  (lisp-unit:assert-first-fp-equal "0.200000000000d+01"
				   (ugaussian-pinv 0.9772498680518208d0))
  (lisp-unit:assert-first-fp-equal "0.200000000000d+01"
				   (ugaussian-qinv 0.02275013194817921d0)))
