;; Gaussian bivariate distribution
;; Liam Healy, Sat Sep  2 2006 - 16:32
;; Time-stamp: <2009-05-24 22:54:07EDT gaussian-bivariate.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'gaussian-bivariate))
     &key sigma-x sigma-y rho)
  "gsl_ran_bivariate_gaussian"
  (((mpointer generator) :pointer) (sigma-x :double) (sigma-y :double) (rho :double)
   (x (:pointer :double)) (y (:pointer :double)))
  :definition :method
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

(defmfun bivariate-gaussian-pdf (x y sigma-x sigma-y rho)
  "gsl_ran_bivariate_gaussian_pdf"
  ((x :double) (y :double) (sigma-x :double) (sigma-y :double) (rho :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x,y) at
   (x,y) for a bivariate Gaussian distribution with standard
   deviations sigma_x, sigma_y and correlation coefficient
   rho, using the formula given for bivariate-gaussian.")

;;; Examples and unit test
(save-test gaussian-bivariate
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect
	    (sample
	     rng 'gaussian-bivariate
	     :sigma-x 1.0d0 :sigma-y 0.75d0 :rho 0.25d0)))
  (bivariate-gaussian-pdf 0.25d0 0.5d0 0.25d0
			   0.4d0 0.2d0))
