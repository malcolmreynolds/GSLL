;; Gaussian bivariate distribution
;; Liam Healy, Sat Sep  2 2006 - 16:32
;; Time-stamp: <2008-02-03 10:30:48EST gaussian-bivariate.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl bivariate-gaussian (generator sigma-x sigma-y rho)
  "gsl_ran_bivariate_gaussian"
  (((generator generator) :pointer) (sigma-x :double) (sigma-y :double) (rho :double)
   (x :double) (y :double))
  :c-return :void
  :documentation			; FDL
  "Generate a pair of correlated Gaussian variates, with
   mean zero, correlation coefficient rho and standard deviations
   sigma_x and sigma_y in the x and y directions.
   The probability distribution for bivariate Gaussian random variates is,
   p(x,y) dx dy
   = {1 \over 2 \pi \sigma_x \sigma_y \sqrt{1-\rho^2}}
   \exp \left(-{(x^2/\sigma_x^2 + y^2/\sigma_y^2 - 2 \rho x y/(\sigma_x\sigma_y))
   \over 2(1-\rho^2)}\right) dx dy
   for x,y in the range -\infty to +\infty.  The
   correlation coefficient rho should lie between 1 and -1.")

(defun-gsl bivariate-gaussian-pdf (x y sigma-x sigma-y rho)
  "gsl_ran_bivariate_gaussian_pdf"
  ((x :double) (y :double) (sigma-x :double) (sigma-y :double) (rho :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x,y) at
   (x,y) for a bivariate Gaussian distribution with standard
   deviations sigma_x, sigma_y and correlation coefficient
   rho, using the formula given for bivariate-gaussian.")

;;; Examples and unit test
(lisp-unit:define-test gaussian-bivariate
  (lisp-unit:assert-equal
   '("-0.650971612449d-01" "-0.157332077491d+01" "0.279427401723d+00"
     "0.120215283589d+01" "-0.604153062691d+00" "0.758270271941d-01"
     "-0.544622941217d+00" "-0.659202684161d+00" "-0.110295166108d+00"
     "0.179318404121d+00" "0.210251049803d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (bivariate-gaussian rng 1.0d0 0.75d0 0.25d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.554826555797d+00"
   (bivariate-gaussian-pdf 0.25d0 0.5d0 0.25d0
			   0.4d0 0.2d0)))
