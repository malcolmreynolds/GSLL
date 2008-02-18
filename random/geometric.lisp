;; Geometric distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-02-17 13:45:43EST geometric.lisp>
;; $Id: $

(in-package :gsl)

(defmfun geometric (generator p)
  "gsl_ran_geometric"
  (((generator generator) :pointer) (p :double))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the geometric distribution,
   the number of independent trials with probability p until the
   first success.  The probability distribution for geometric variates
   is p(k) =  p (1-p)^{k-1} for k >= 1.
   Note that the distribution begins with k=1 with this
   definition.  There is another convention in which the exponent k-1
   is replaced by k.")

(defmfun geometric-pdf (k p)
  "gsl_ran_geometric_pdf" ((k :uint) (p :double))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a geometric distribution with probability parameter p, using
   the formula given in #'geometric.")

(defmfun geometric-P (k p)
  "gsl_cdf_geometric_P" ((k :uint) (p :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the geometric distribution with parameter p.")

(defmfun geometric-Q (k p)
  "gsl_cdf_geometric_Q" ((k :uint) (p :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(k) for the geometric distribution with parameters p.")

;;; Examples and unit test
#|
(make-tests geometric
  (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (geometric rng 0.4d0)))
  (geometric-pdf 2 0.4d0)
  (geometric-P 2 0.4d0)
  (geometric-Q 2 0.4d0))
|#

(LISP-UNIT:DEFINE-TEST GEOMETRIC
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 1 4 3 1 3 2 1 1 2 1 1))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (GEOMETRIC RNG 0.4d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.24d0)
   (MULTIPLE-VALUE-LIST (GEOMETRIC-PDF 2 0.4d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.64d0)
   (MULTIPLE-VALUE-LIST (GEOMETRIC-P 2 0.4d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.36d0)
   (MULTIPLE-VALUE-LIST (GEOMETRIC-Q 2 0.4d0))))

