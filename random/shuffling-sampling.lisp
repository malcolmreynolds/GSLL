;; Shuffling and sampling
;; Liam Healy, Sat Dec  2 2006 - 18:40
;; Time-stamp: <2009-05-24 23:13:40EDT shuffling-sampling.lisp>
;; $Id$

(in-package :gsl)

;;; These are currently defined only for vectors.

(defmfun sample
    ((generator random-number-generator) (type (eql 'shuffle))
     &key base)
  "gsl_ran_shuffle"
  (((mpointer generator) :pointer)
   ((c-pointer base) :pointer) ((dim0 base) sizet) ((element-size base) sizet))
  :definition :method
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

(defmfun sample
    ((generator random-number-generator) (type (eql 'choose-random))
     &key src (dest (dim0 src))
     &aux
     (destarr
      (if (integerp dest)
	  (make-marray (element-type src) :dimensions dest)
	  dest)))
  "gsl_ran_choose"
  (((mpointer generator) :pointer)
   ((c-pointer destarr) :pointer) ((dim0 destarr) sizet)
   ((c-pointer src) :pointer) ((dim0 src) sizet) ((element-size src) sizet))
  :definition :method
  :inputs (destarr src)
  :outputs (destarr)
  :documentation			; FDL
  "Fill the array dest[k] with k objects taken randomly from the n
   elements of the array src[0...n-1].  The output of the random
   number generator r is used to make the selection.  The algorithm
   ensures all possible samples are equally likely, assuming a perfect
   source of randomness.

   The objects are sampled without replacement, thus each object can
   only appear once in dest[k].  It is required that k be less
   than or equal to n.  The objects in dest will be in the
   same relative order as those in src.  You will need to call
   with 'shuffle if you want to randomize the order.")

(defmfun sample
    ((generator random-number-generator) (type (eql 'random-sample))
     &key src (dest (dim0 src))
     &aux
     (destarr
      (if (integerp dest)
	  (make-marray (element-type src) :dimensions dest)
	  dest)))
  "gsl_ran_sample"
  (((mpointer generator) :pointer)
   ((c-pointer destarr) :pointer) ((dim0 destarr) sizet)
   ((c-pointer src) :pointer) ((dim0 src) sizet) ((element-size src) sizet))
  :definition :method
  :inputs (destarr src)
  :outputs (destarr)
  :c-return :void
  :documentation
  "Like #'choose-random, but samples k items
   from the original array of n items src with replacement, so
   the same object can appear more than once in the output sequence
   dest.  There is no requirement that k be less than n
   in this case.")

;;; Examples and unit test
(save-test shuffling-sampling
 (let ((rng (make-random-number-generator +mt19937+ 0))
       (v1 #31m(1 2 3 4 5 6 7 8)))
   (cl-array (sample rng 'shuffle :base v1)))
 (let ((rng (make-random-number-generator +mt19937+ 0))
       (v1 #31m(1 2 3 4 5 6 7 8)))
   (cl-array (sample rng 'choose-random :src v1 :dest 4)))
 (let ((rng (make-random-number-generator +mt19937+ 0))
       (v1 #31m(1 2 3 4 5 6 7 8)))
   (cl-array (sample rng 'random-sample :src v1 :dest 10))))
