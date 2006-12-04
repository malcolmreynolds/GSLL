;********************************************************
; file:        shuffling-sampling.lisp                   
; description: Shuffling and sampling                    
; date:        Sat Dec  2 2006 - 18:40                   
; author:      Liam M. Healy                             
; modified:    Sat Dec  2 2006 - 18:55
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; These are defined for sequences of memory locations and
;;; so are difficult to adapt to arbitrary CL objects.
;;; The interfaces will probably change.

(defun-gsl shuffle (generator base n size)
  "gsl_ran_shuffle"
  (((generator generator) :pointer) (base :pointer) (n :size) (size :size))
  :c-return :void
  :documentation
  "Randomly shuffle the order of @var{n} objects, each of
   size @var{size}, stored in the array @var{base}[0..@var{n}-1].  The
   output of the random number generator @var{r} is used to produce the
   permutation.  The algorithm generates all possible @math{n!}
   permutations with equal probability, assuming a perfect source of random
   numbers.")

(defun-gsl choose-random (generator dest k src n size)
  "gsl_ran_choose"
  (((generator generator) :pointer)
   (dest :pointer) (k :size) (src :pointer) (n :size) (size :size))
  ;; This is described in the GSL docs as returning int, but it does
  ;; not say what the return value means.  Therefore, we ignore it.
  :c-return :void
  :documentation
  "Fill the array @var{dest}[k] with @var{k} objects taken
   randomly from the @var{n} elements of the array
   @var{src}[0..@var{n}-1].  The objects are each of size @var{size}.  The
   output of the random number generator @var{r} is used to make the
   selection.  The algorithm ensures all possible samples are equally
   likely, assuming a perfect source of randomness.

   The objects are sampled @emph{without} replacement, thus each object can
   only appear once in @var{dest}[k].  It is required that @var{k} be less
   than or equal to @code{n}.  The objects in @var{dest} will be in the
   same relative order as those in @var{src}.  You will need to call
   @code{gsl_ran_shuffle(r, dest, n, size)} if you want to randomize the
   order.")

(defun-gsl sample (generator dest k src n size)
  "gsl_ran_sample"
  (((generator generator) :pointer)
   (dest :pointer) (k :size) (src :pointer) (n :size) (size :size))
  :c-return :void
  :documentation
  "Like #'choose-random, but samples @var{k} items
   from the original array of @var{n} items @var{src} with replacement, so
   the same object can appear more than once in the output sequence
   @var{dest}.  There is no requirement that @var{k} be less than @var{n}
   in this case.")

