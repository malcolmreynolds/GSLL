;; Discrete random variables
;; Liam Healy, Sat Nov 11 2006 - 21:51
;; Time-stamp: <2008-02-17 13:36:26EST discrete.lisp>
;; $Id: $

(in-package :gsl)

(cffi:defcstruct discrete-t
  "Structure for Walker algorithm."
  (K size)
  (A :pointer)
  (F :pointer))

(defgo-s (discrete-random probabilities) discrete-preprocess discrete-free)

(defmfun discrete-preprocess (probabilities) 
  "gsl_ran_discrete_preproc"
  (((dim0 probabilities) size) ((gsl-array probabilities) :pointer))
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
  :c-return size
  :documentation			; FDL
  "Generate discrete random numbers after running #'discrete-preprocess;
   the argument 'table is the value returned by #'discrete-preprocess.")

(defmfun discrete-pdf (k table)
  "gsl_ran_discrete_pdf"
  ((k size) (table :pointer))
  :c-return :double
  :documentation			; FDL
  "The probability P[k] of observing the variable k.
   Since P[k] is not stored as part of the lookup table, it must be
   recomputed; this computation takes O(K), so if K is large
   and you care about the original array P[k] used to create the
   lookup table, then you should just keep this original array P[k]
   around.")

;;; Examples and unit test
#|
(make-tests discrete
   ;; Must have two letms because the vector value is not set until
   ;; the body, but the discrete-random needs that set value.
   (letm ((probabilities (vector-double #(0.25d0 0.5d0 0.25d0))))
     (letm ((table (discrete-random probabilities))
	    (rng (random-number-generator *mt19937* 0)))
       (loop for i from 0 to 10
	     collect
	     (discrete rng table))))
   (letm ((probabilities (vector-double #(0.25d0 0.5d0 0.25d0))))
      (letm ((table (discrete-random probabilities)))
	(discrete-pdf 1 table))))
|#

(LISP-UNIT:DEFINE-TEST DISCRETE
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 1 0 1 1 0 1 1 2 1 2 2))
   (MULTIPLE-VALUE-LIST
    (LETM ((PROBABILITIES
	    (VECTOR-DOUBLE #(0.25d0 0.5d0 0.25d0))))
      (LETM ((TABLE (DISCRETE-RANDOM PROBABILITIES))
	     (RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
	(LOOP FOR I FROM 0 TO 10 COLLECT
	      (DISCRETE RNG TABLE))))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((PROBABILITIES (VECTOR-DOUBLE #(0.25d0 0.5d0 0.25d0))))
      (LETM ((TABLE (DISCRETE-RANDOM PROBABILITIES)))
	(DISCRETE-PDF 1 TABLE))))))

