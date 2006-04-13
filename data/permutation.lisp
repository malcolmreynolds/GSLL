;********************************************************
; file:        permutation.lisp                        
; description: Permutations
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Wed Apr 12 2006 - 23:51
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Permutation object definition, allocation, reading & writing
;;;;****************************************************************************

;;; GSL-permutation definition
(cffi:defcstruct gsl-permutation-c
  (size :size)
  (data :pointer))

;;; Allocation, freeing, reading and writing
(gsl-data-functions "permutation" :size)

(add-wrap-type gsl-permutation-c (lambda (x) `(pointer ,x)))

(defun-gsl set-identity (((pointer permutation) gsl-permutation-c))
     "gsl_permutation_init"
     :method ((permutation gsl-permutation))
     :documentation
     "Initialize the permutation @var{p} to the identity, i.e.
   @math{(0,1,2,@dots{},n-1)}.")

(defun-gsl permutation-copy
    ((destination gsl-permutation-c) (source gsl-permutation-c) )
  "gsl_permutation_memcpy"
  :documentation
  "Copy the elements of the permutation @var{src} into the
permutation @var{dest}.  The two permutations must have the same size.")

;;;;****************************************************************************
;;;; Accessing elements
;;;;****************************************************************************

(defun-gsl gsl-aref
    (((pointer permutation) :pointer) ((first indices) :size))
  "gsl_permutation_get"
  :method ((permutation gsl-permutation) &rest indices)
  :return (:double)
  :c-return-value :return
  :documentation "The ith element of the permutation.")

(defun-gsl permutation-swap ((p gsl-permutation-c) (i :size) (j :size))
  "gsl_permutation_swap"
  :documentation
  "Exchanges the @var{i}-th and @var{j}-th elements of the
   permutation @var{p}.")

;;;;****************************************************************************
;;;; Permutation properties
;;;;****************************************************************************

(defun-gsl permutation-size ((p gsl-permutation-c))
  "gsl_permutation_size"
  :c-return-value :return
  :return (:size) 
  :documentation
  "The size of the permutation @var{p}.")

(defun-gsl permutation-data ((p gsl-permutation-c))
  "gsl_permutation_data"
  :c-return-value :return
  :return (:pointer) 
  :documentation
  "A pointer to the array of elements in the
   permutation @var{p}.")

(defun-gsl data-valid (((pointer permutation) :pointer))
  "gsl_permutation_valid"
  :method ((permutation gsl-permutation))
  :c-return-value :return
  :return (:boolean) 
  :documentation
  "Check that the permutation @var{p} is valid.  The @var{n}
elements should contain each of the numbers 0 to @math{@var{n}-1} once and only
once.")

;;;;****************************************************************************
;;;; Permutation functions
;;;;****************************************************************************

(defun-gsl permutation-reverse ((p gsl-permutation-c))
  "gsl_permutation_reverse"
  :c-return-value :void
  :documentation
  "Reverse the order of the elements of the permutation @var{p}.")

(defun-gsl permutation-inverse ((inv gsl-permutation-c) (p gsl-permutation-c))
  "gsl_permutation_inverse"
  :documentation
  "Reverse the order of the elements of the permutation @var{p}.")

(defun-gsl permutation-next ((p gsl-permutation-c))
  "gsl_permutation_next"
  :c-return-value :success-failure
  :documentation
  "Advance the permutation @var{p} to the next permutation
   in lexicographic order and return T.  If no further
   permutations are available, return NIL and leave
   @var{p} unmodified.  Starting with the identity permutation and
   repeatedly applying this function will iterate through all possible
   permutations of a given order.")

(defun-gsl permutation-prev ((p gsl-permutation-c))
  "gsl_permutation_prev"
  :c-return-value :success-failure
  :documentation
  "Step backwards from the permutation @var{p} to the
   previous permutation in lexicographic order, returning T.
   If no previous permutation is available, return
   NIL and leaves @var{p} unmodified.")

;;;;****************************************************************************
;;;; Applying Permutations
;;;;****************************************************************************

(defun-gsl permute
    ((p gsl-permutation-c) (data :pointer) (stride :size) (n :size))
  "gsl_permute"
  :documentation
  "Apply the permutation @var{p} to the array @var{data} of
   size @var{n} with stride @var{stride}.")

(defun-gsl permute-inverse
    ((p gsl-permutation-c) (data :pointer) (stride :size) (n :size))
  "gsl_permute_inverse"
  :documentation
  "Apply the inverse of the permutation @var{p} to the array @var{data} of
   size @var{n} with stride @var{stride}.")

(defun-gsl permute-vector ((p gsl-permutation-c) (v gsl-vector-c))
  "gsl_permute_vector"
  :documentation
  "Apply the permutation @var{p} to the elements of the
   vector @var{v}, considered as a row-vector acted on by a permutation
   matrix from the right, @math{v' = v P}.  The @math{j}-th column of the
   permutation matrix @math{P} is given by the @math{p_j}-th column of the
   identity matrix. The permutation @var{p} and the vector @var{v} must
   have the same length.")

(defun-gsl permute-vector-inverse ((p gsl-permutation-c) (v gsl-vector-c))
  "gsl_permute_vector_inverse"
  :documentation
  "Apply the inverse of the permutation @var{p} to the
  elements of the vector @var{v}, considered as a row-vector acted on by
  an inverse permutation matrix from the right, @math{v' = v P^T}.  Note
  that for permutation matrices the inverse is the same as the transpose.
  The @math{j}-th column of the permutation matrix @math{P} is given by
  the @math{p_j}-th column of the identity matrix. The permutation @var{p}
  and the vector @var{v} must have the same length.")

(defun-gsl permutation*
    ((p gsl-permutation-c) (pa gsl-permutation-c) (pb gsl-permutation-c))
  "gsl_permutation_mul"
  :documentation
  "Combine the two permutations @var{pa} and @var{pb} into a
  single permutation @var{p}, where @math{p = pa . pb}. The permutation
  @var{p} is equivalent to applying @math{pb} first and then @var{pa}.")

;;;;****************************************************************************
;;;; Permutations in cyclic form
;;;;****************************************************************************

(defun-gsl linear-to-canonical ((q gsl-permutation-c) (p gsl-permutation-c))
  "gsl_permutation_linear_to_canonical"
  :documentation
  "Compute the canonical form of the permutation @var{p} and
   stores it in the output argument @var{q}.")

(defun-gsl canonical-to-linear ((p gsl-permutation-c) (q gsl-permutation-c))
  "gsl_permutation_canonical_to_linear"
  :documentation
  "Convert a permutation @var{q} in canonical form back into
   linear form storing it in the output argument @var{p}.")

(defun-gsl inversions ((p gsl-permutation-c))
  "gsl_permutation_inversions"
  :c-return-value :return
  :return (:size)
  :documentation
  "Count the number of inversions in the permutation
  @var{p}.  An inversion is any pair of elements that are not in order.
  For example, the permutation 2031 has three inversions, corresponding to
  the pairs (2,0) (2,1) and (3,1).  The identity permutation has no
  inversions.")

(defun-gsl linear-cycles ((p gsl-permutation-c))
  "gsl_permutation_linear_cycles"
  :c-return-value :return
  :return (:size)
  :documentation
  "Count the number of cycles in the permutation @var{p}, given in linear form.")

(defun-gsl canonical-cycles ((p gsl-permutation-c))
  "gsl_permutation_canonical_cycles"
  :c-return-value :return
  :return (:size)
  :documentation
  "Count the number of cycles in the permutation @var{q},
   given in canonical form.")

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

#|

|#
