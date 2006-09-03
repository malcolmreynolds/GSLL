;********************************************************
; file:        gaussian-bivariate.lisp                   
; description: Gaussian bivariate distribution           
; date:        Sat Sep  2 2006 - 16:32                   
; author:      Liam M. Healy                             
; modified:    Sat Sep  2 2006 - 21:34
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl bivariate-gaussian (generator sigma-x sigma-y rho)
  "gsl_ran_bivariate_gaussian"
  (((generator generator) :pointer) (sigma-x :double) (sigma-y :double) (rho :double)
   (x :double) (y :double))
  :c-return :void
  :documentation
  "Generate a pair of correlated Gaussian variates, with
   mean zero, correlation coefficient @var{rho} and standard deviations
   @var{sigma_x} and @var{sigma_y} in the @math{x} and @math{y} directions.
   The probability distribution for bivariate Gaussian random variates is,
   p(x,y) dx dy
   = {1 \over 2 \pi \sigma_x \sigma_y \sqrt{1-\rho^2}}
   \exp \left(-{(x^2/\sigma_x^2 + y^2/\sigma_y^2 - 2 \rho x y/(\sigma_x\sigma_y))
   \over 2(1-\rho^2)}\right) dx dy
   for @math{x,y} in the range @math{-\infty} to @math{+\infty}.  The
   correlation coefficient @var{rho} should lie between @math{1} and @math{-1}.")

(defun-gsl bivariate-gaussian-pdf (x y sigma-x sigma-y rho)
  "gsl_ran_bivariate_gaussian_pdf"
  ((x :double) (y :double) (sigma-x :double) (sigma-y :double) (rho :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x,y)} at
   (@var{x},@var{y}) for a bivariate Gaussian distribution with standard
   deviations @var{sigma_x}, @var{sigma_y} and correlation coefficient
   @var{rho}, using the formula given for bivariate-gaussian.")

;;; Examples and unit test
(lisp-unit:define-test gaussian-bivariate
  (lisp-unit:assert-equal
   '("-0.650971612449d-01" "-0.157332077491d+01" "0.279427401723d+00"
     "0.120215283589d+01" "-0.604153062691d+00" "0.758270271941d-01"
     "-0.544622941217d+00" "-0.659202684161d+00" "-0.110295166108d+00"
     "0.179318404121d+00" "0.210251049803d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (bivariate-gaussian *rng-mt19937* 1.0d0 0.75d0 0.25d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.554826555797d+00"
   (bivariate-gaussian-pdf 0.25d0 0.5d0 0.25d0
			   0.4d0 0.2d0)))
