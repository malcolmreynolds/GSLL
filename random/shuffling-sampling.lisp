;; Shuffling and sampling
;; Liam Healy, Sat Dec  2 2006 - 18:40
;; Time-stamp: <2008-12-29 19:36:17EST shuffling-sampling.lisp>
;; $Id$

(in-package :gsl)

;;; These are currently defined only for vectors.

(defmfun shuffle (generator base)
  "gsl_ran_shuffle"
  (((mpointer generator) :pointer)
   ((c-pointer base) :pointer) ((dim0 base) sizet) ((element-size base) sizet))
  :inputs (base)
  :outputs (base)
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
  (((mpointer generator) :pointer)
   ((c-pointer dest) :pointer) ((dim0 dest) sizet)
   ((c-pointer src) :pointer) ((dim0 src) sizet) ((element-size src) sizet))
  :inputs (dest src)
  :outputs (dest)
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

(defmfun random-sample (generator dest src)
  "gsl_ran_sample"
  (((mpointer generator) :pointer)
   ((c-pointer dest) :pointer) ((dim0 dest) sizet)
   ((c-pointer src) :pointer) ((dim0 src) sizet) ((element-size src) sizet))
  :inputs (dest src)
  :outputs (dest)
  :c-return :void
  :documentation
  "Like #'choose-random, but samples k items
   from the original array of n items src with replacement, so
   the same object can appear more than once in the output sequence
   dest.  There is no requirement that k be less than n
   in this case.")

;;; Examples and unit test
(save-test shuffling-sampling
 (let ((rng (make-random-number-generator *mt19937* 0))
	(v1 #31m(1 2 3 4 5 6 7 8)))
   (shuffle rng v1)
   (cl-array v1))
 (let ((rng (make-random-number-generator *mt19937* 0))
	(v1 #31m(1 2 3 4 5 6 7 8))
	(v2 (make-marray '(signed-byte 32) :dimensions 4)))
   (choose-random rng v2 v1)
   (cl-array v2))
 (let ((rng (make-random-number-generator *mt19937* 0))
	(v1 #31m(1 2 3 4 5 6 7 8))
	(v2 (make-marray '(signed-byte 32) :dimensions 10)))
   (random-sample rng v2 v1)
   (cl-array v2)))
