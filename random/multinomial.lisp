;; Multinomial distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-02-17 18:31:31EST multinomial.lisp>
;; $Id$

(in-package :gsl)

(defmfun multinomial (generator sum p n)
  "gsl_ran_multinomial"
  (((generator generator) :pointer) 
   ((dim0 p) size)
   (sum size)
   ((gsl-array p) :pointer)
   ;; technically, n should be a uint array, but integers work
   ((gsl-array n) :pointer))
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
  (((dim0 p) :uint) ((gsl-array p) :pointer) ((gsl-array n) :pointer))
  :c-return :double
  :documentation			; FDL
  "Compute the probability P(n_1, n_2, ..., n_K)
   of sampling n[K] from a multinomial distribution 
   with parameters p[K], using the formula given for #'multinomial.")

(defmfun multinomial-log-pdf (p n)
  "gsl_ran_multinomial_lnpdf"
  (((dim0 p) :uint) ((gsl-array p) :pointer) ((gsl-array n) :pointer))
  :c-return :double
  :documentation			; FDL
  "Compute the natural logarithm of the probability P(n_1, n_2, ..., n_K)
   of sampling n[K] from a multinomial distribution 
   with parameters p[K], using the formula given for #'multinomial.")

;;; Examples and unit test
#|
(make-tests multinomial
  (letm ((rng (random-number-generator *mt19937* 0))
	  (p (vector-double #(0.1d0 0.2d0 0.3d0 0.4d0)))
	  (n (vector-fixnum 4)))
     (multinomial rng 8 p n)
     (data n))
  (letm ((p (vector-double #(0.1d0 0.2d0 0.3d0 0.4d0)))
	  (n (vector-fixnum 4)))
     (setf (data n) #(5 0 1 2))
     (multinomial-pdf p N))
  (letm ((p (vector-double #(0.1d0 0.2d0 0.3d0 0.4d0)))
	  (n (vector-fixnum 4)))
     (setf (data n) #(5 0 1 2))
     (multinomial-log-pdf p n)))
|#

(LISP-UNIT:DEFINE-TEST MULTINOMIAL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(5 0 1 2))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0))
	 (P (VECTOR-DOUBLE #(0.1d0 0.2d0 0.3d0 0.4d0)))
	 (N (VECTOR-FIXNUM 4)))
      (MULTINOMIAL RNG 8 P N)
      (DATA N))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 8.064000000000026d-5)
   (MULTIPLE-VALUE-LIST
    (LETM
	((P (VECTOR-DOUBLE #(0.1d0 0.2d0 0.3d0 0.4d0)))
	 (N (VECTOR-FIXNUM 4)))
      (SETF (DATA N) #(5 0 1 2)) (MULTINOMIAL-PDF P N))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -9.425515753641212d0)
   (MULTIPLE-VALUE-LIST
    (LETM
	((P (VECTOR-DOUBLE #(0.1d0 0.2d0 0.3d0 0.4d0)))
	 (N (VECTOR-FIXNUM 4)))
      (SETF (DATA N) #(5 0 1 2))
      (MULTINOMIAL-LOG-PDF P N)))))

