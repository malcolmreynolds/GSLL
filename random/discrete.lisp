;; Discrete random variables
;; Liam Healy, Sat Nov 11 2006 - 21:51
;; Time-stamp: <2009-01-25 09:54:54EST discrete.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(defmobject discrete-random
    ("gsl_ran_discrete" "gsl_ran_discrete_preproc" (probabilities))
  (((dim0 probabilities) sizet) ((c-pointer probabilities) :pointer))
  "lookup table for the discrete random number generator"
  :documentation			; FDL
  "Make a structure that contains the lookup
  table for the discrete random number generator.  The array probabilities contains
  the probabilities of the discrete events; these array elements must all be 
  positive, but they needn't add up to one (so you can think of them more
  generally as ``weights'')---the preprocessor will normalize appropriately.
  This return value is used as an argument to #'discrete.")

(defmfun discrete (generator table)
  "gsl_ran_discrete"
  (((mpointer generator) :pointer) ((mpointer table) :pointer))
  :c-return sizet
  :documentation
  "Generate discrete random numbers.")

(defmfun discrete-pdf (k table)
  "gsl_ran_discrete_pdf"
  ((k sizet) ((mpointer table) :pointer))
  :c-return :double
  :documentation			; FDL
  "The probability P[k] of observing the variable k.
   Since P[k] is not stored as part of the lookup table, it must be
   recomputed; this computation takes O(K), so if K is large
   and you care about the original array P[k] used to create the
   lookup table, then you should just keep this original array P[k]
   around.")

;;; Examples and unit test
(save-test discrete
 (let* ((probabilities #m(0.25d0 0.5d0 0.25d0))
	(table (make-discrete-random probabilities))
	(rng (make-random-number-generator *mt19937* 0)))
   (loop for i from 0 to 10
      collect
      (discrete rng table)))
 (let* ((probabilities #m(0.25d0 0.5d0 0.25d0))
	(table (make-discrete-random probabilities)))
   (discrete-pdf 1 table)))
