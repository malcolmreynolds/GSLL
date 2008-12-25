;; Discrete random variables
;; Liam Healy, Sat Nov 11 2006 - 21:51
;; Time-stamp: <2008-12-25 11:55:10EST discrete.lisp>
;; $Id$

(in-package :gsl)

#|
(defmobject discrete-random
    ("gsl_ran_discrete" "gsl_ran_discrete_preproc")
  (((dim0 probabilities) sizet) ((c-pointer probabilities) :pointer))
  "lookup table for the discrete random number generator" ; FDL
  "Make a structure that contains the lookup
  table for the discrete random number generator.  The array probabilities contains
  the probabilities of the discrete events; these array elements must all be 
  positive, but they needn't add up to one (so you can think of them more
  generally as ``weights'')---the preprocessor will normalize appropriately.
  This return value is used as an argument to #'discrete.")
|#

(defgo-s (discrete-random probabilities) discrete-preprocess discrete-free)

(defmfun discrete-preprocess (probabilities) 
  "gsl_ran_discrete_preproc"
  (((dim0 probabilities) sizet) ((c-pointer probabilities) :pointer))
  :c-return :pointer
  :export nil
  :index (letm discrete-random)
  :documentation			; FDL
  "A pointer to a structure that contains the lookup
  table for the discrete random number generator.  The array probabilities contains
  the probabilities of the discrete events; these array elements must all be 
  positive, but they needn't add up to one (so you can think of them more
  generally as ``weights'')---the preprocessor will normalize appropriately.
  This return value is used as an argument to #'discrete.")

(defmfun discrete-free (table)
  "gsl_ran_discrete_free" ((table :pointer))
  :c-return :void
  :export nil
  :index (letm discrete-random)
  :documentation			; FDL
  "De-allocates the lookup table created by #'discrete-preprocess.")

(defmfun discrete (generator table)
  "gsl_ran_discrete"
  (((generator generator) :pointer) (table :pointer))
  :c-return sizet
  :documentation			; FDL
  "Generate discrete random numbers after running #'discrete-preprocess;
   the argument 'table is the value returned by #'discrete-preprocess.")

(defmfun discrete-pdf (k table)
  "gsl_ran_discrete_pdf"
  ((k sizet) (table :pointer))
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
   ;; Must have two letms because the vector value is not set until
   ;; the body, but the discrete-random needs that set value.
   (letm ((probabilities #m(0.25d0 0.5d0 0.25d0)))
     (letm ((table (discrete-random probabilities))
	    (rng (random-number-generator *mt19937* 0)))
       (loop for i from 0 to 10
	     collect
	     (discrete rng table))))
   (letm ((probabilities #m(0.25d0 0.5d0 0.25d0)))
      (letm ((table (discrete-random probabilities)))
	(discrete-pdf 1 table))))
