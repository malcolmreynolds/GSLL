;; Shuffling and sampling
;; Liam Healy, Sat Dec  2 2006 - 18:40
;; Time-stamp: <2008-09-14 21:41:43EDT shuffling-sampling.lisp>
;; $Id$

(in-package :gsl)

;;; These are currently defined only for vectors.

(defmfun shuffle (generator base)
  "gsl_ran_shuffle"
  (((generator generator) :pointer)
   ((c-pointer base) :pointer) ((dim0 base) sizet) ((element-size base) sizet))
  :c-return :void
  :documentation			; FDL
  "Randomly shuffle the order of n objects, each of
   size size, stored in the array base[0...n-1].  The
   output of the random number generator r is used to produce the
   permutation.  The algorithm generates all possible n!
   permutations with equal probability, assuming a perfect source of random
   numbers.")

(defmfun choose-random (generator dest src)
  "gsl_ran_choose"
  (((generator generator) :pointer)
   ((c-pointer dest) :pointer) ((dim0 dest) sizet)
   ((c-pointer src) :pointer) ((dim0 src) sizet) ((element-size src) sizet))
  ;; This is described in the GSL docs as returning int, but it does
  ;; not say what the return value means.  Therefore, we ignore it.
  :c-return :void
  :documentation			; FDL
  "Fill the array dest[k] with k objects taken
   randomly from the n elements of the array
   src[0...n-1].  The objects are each of size size.  The
   output of the random number generator r is used to make the
   selection.  The algorithm ensures all possible samples are equally
   likely, assuming a perfect source of randomness.

   The objects are sampled without replacement, thus each object can
   only appear once in dest[k].  It is required that k be less
   than or equal to n.  The objects in dest will be in the
   same relative order as those in src.  You will need to call
   #'shuffle if you want to randomize the order.")

(defmfun sample  (generator dest src)
  "gsl_ran_sample"
  (((generator generator) :pointer)
   ((c-pointer dest) :pointer) ((dim0 dest) sizet)
   ((c-pointer src) :pointer) ((dim0 src) sizet) ((element-size src) sizet))
  :c-return :void
  :documentation
  "Like #'choose-random, but samples k items
   from the original array of n items src with replacement, so
   the same object can appear more than once in the output sequence
   dest.  There is no requirement that k be less than n
   in this case.")

;;; Examples and unit test
#|
(make-tests shuffling-sampling
 (letm ((rng (random-number-generator *mt19937* 0))
	(v1 (vector-fixnum #(1 2 3 4 5 6 7 8))))
   (shuffle rng v1)
   (data v1))
 (letm ((rng (random-number-generator *mt19937* 0))
	(v1 (vector-fixnum #(1 2 3 4 5 6 7 8)))
	(v2 (vector-fixnum 4)))
   (choose-random rng v2 v1)
   (data v2))
 (letm ((rng (random-number-generator *mt19937* 0))
	(v1 (vector-fixnum #(1 2 3 4 5 6 7 8)))
	(v2 (vector-fixnum 10)))
   (sample rng v2 v1)
   (data v2)))

(LISP-UNIT:DEFINE-TEST SHUFFLING-SAMPLING
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(4 3 6 1 5 7 2 8))
   (MULTIPLE-VALUE-LIST
    (LETM
	((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0))
	 (V1 (VECTOR-FIXNUM #(1 2 3 4 5 6 7 8))))
      (SHUFFLE RNG V1) (DATA V1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(2 3 5 8))
   (MULTIPLE-VALUE-LIST
    (LETM
	((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0))
	 (V1 (VECTOR-FIXNUM #(1 2 3 4 5 6 7 8)))
	 (V2 (VECTOR-FIXNUM 4)))
      (CHOOSE-RANDOM RNG V2 V1)
      (DATA V2))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(8 2 3 8 2 4 8 6 5 6))
   (MULTIPLE-VALUE-LIST
    (LETM
	((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0))
	 (V1 (VECTOR-FIXNUM #(1 2 3 4 5 6 7 8)))
	 (V2 (VECTOR-FIXNUM 10)))
      (SAMPLE RNG V2 V1) (DATA V2)))))
|#
