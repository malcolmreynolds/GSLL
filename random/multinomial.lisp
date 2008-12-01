;; Multinomial distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-11-30 23:19:48EST multinomial.lisp>
;; $Id$

(in-package :gsl)

(defmfun multinomial (generator sum p n)
  "gsl_ran_multinomial"
  (((generator generator) :pointer) 
   ((dim0 p) sizet)
   (sum sizet)
   ((c-pointer p) :pointer)
   ;; technically, n should be a uint array, but integers work
   ((c-pointer n) :pointer))
  :c-return :void
  :documentation			; FDL
  "Returns an array n of K random variates from a 
   multinomial distribution.  The sum of the array n is specified
   by sum=N.  The distribution function is
   P(n_1, n_2, ..., n_K) = 
   (N!/(n_1! n_2! ... n_K!)) p_1^n_1 p_2^n_2 ... p_K^n_K
   where (n_1, n_2, ..., n_K) are nonnegative integers with 
   sum_{k=1}^K n_k = N, and (p_1, p_2, ..., p_K)
   is a probability distribution with \sum p_i = 1.  
   If the array p[K] is not normalized then its entries will be
   treated as weights and normalized appropriately.
   Random variates are generated using the conditional binomial method (see
   C.S. David, \"The computer generation of multinomial random
   variates,\" Comp. Stat. Data Anal. 16 (1993) 205--217 for details).")

(defmfun multinomial-pdf (p n)
  "gsl_ran_multinomial_pdf"
  (((dim0 p) sizet) ((c-pointer p) :pointer) ((c-pointer n) :pointer))
  :c-return :double
  :documentation			; FDL
  "Compute the probability P(n_1, n_2, ..., n_K)
   of sampling n[K] from a multinomial distribution 
   with parameters p[K], using the formula given for #'multinomial.")

(defmfun multinomial-log-pdf (p n)
  "gsl_ran_multinomial_lnpdf"
  (((dim0 p) sizet) ((c-pointer p) :pointer) ((c-pointer n) :pointer))
  :c-return :double
  :documentation			; FDL
  "Compute the natural logarithm of the probability P(n_1, n_2, ..., n_K)
   of sampling n[K] from a multinomial distribution 
   with parameters p[K], using the formula given for #'multinomial.")

;;; Examples and unit test
(save-test multinomial
 (letm ((rng (random-number-generator *mt19937* 0))
	(p #m(0.1d0 0.2d0 0.3d0 0.4d0))
	(n (make-array* '(signed-byte 32) :dimensions 4)))
   (multinomial rng 8 p n)
   (cl-array n))
 (letm ((p #m(0.1d0 0.2d0 0.3d0 0.4d0))
	(n #31m(5 0 1 2)))
   (multinomial-pdf p N))
 (letm ((p #m(0.1d0 0.2d0 0.3d0 0.4d0))
	(n #31m(5 0 1 2)))
   (multinomial-log-pdf p n)))
