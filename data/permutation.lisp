;********************************************************
; file:        permutation.lisp                        
; description: Permutations
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Sat Jun  3 2006 - 22:04
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Permutation structure and CL object
;;;;****************************************************************************

;;; GSL-permutation definition
(cffi:defcstruct gsl-permutation-c
  (size :size)
  (data :pointer))

;;; Allocation, freeing, reading and writing
(defdata "permutation" permutation fixnum)

(defmethod gsl-array ((object gsl-permutation))
  (foreign-slot-value (pointer object) 'gsl-permutation-c 'data))

;;;;****************************************************************************
;;;; Getting values
;;;;****************************************************************************

(defun-gsl gsl-aref ((permutation gsl-permutation) &rest indices)
  "gsl_permutation_get"
  (((pointer permutation) :pointer) ((first indices) :size))
  :type :method 
  :c-return :size
  :documentation "The ith element of the permutation.")

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defun-gsl set-identity ((permutation gsl-permutation))
  "gsl_permutation_init"
  (((pointer permutation) :pointer))
  :type :method
  :c-return :void
  :documentation
  "Initialize the permutation @var{p} to the identity, i.e.
   @math{(0,1,2,@dots{},n-1)}.")

(defun-gsl copy ((destination gsl-permutation) (source gsl-permutation))
  "gsl_permutation_memcpy"
  (((pointer destination) gsl-permutation-c)
   ((pointer source) gsl-permutation-c))
  :type :method
  :invalidate (destination)
  :documentation
  "Copy the elements of the permutation @var{src} into the
   permutation @var{dest}.  The two permutations must have the same size.")

(defun-gsl swap-elements ((p gsl-permutation) i j)
  "gsl_permutation_swap"
  (((pointer p) gsl-permutation-c) (i :size) (j :size))
  :type :method
  :invalidate (p)
  :documentation
  "Exchanges the @var{i}-th and @var{j}-th elements of the
   permutation @var{p}.")

;;;;****************************************************************************
;;;; Permutation properties
;;;;****************************************************************************

(defun-gsl permutation-size (p)
  "gsl_permutation_size"
  (((pointer p) gsl-permutation-c))
  :c-return :size
  :documentation
  "The size of the permutation @var{p}.")

(defun-gsl permutation-data (p)
  "gsl_permutation_data"
  (((pointer p) gsl-permutation-c))
  :c-return :pointer
  :documentation
  "A pointer to the array of elements in the
   permutation @var{p}.")

(defun-gsl data-valid ((permutation gsl-permutation))
  "gsl_permutation_valid"
  (((pointer permutation) :pointer))
  :type :method 
  :c-return :boolean
  :documentation
  "Check that the permutation @var{p} is valid.  The @var{n}
  elements should contain each of the numbers 0 to @math{@var{n}-1} once and only
  once.")

;;;;****************************************************************************
;;;; Permutation functions
;;;;****************************************************************************

(defun-gsl permutation-reverse (p)
  "gsl_permutation_reverse"
  (((pointer p) gsl-permutation-c))
  :invalidate (p)
  :c-return :void
  :documentation
  "Reverse the order of the elements of the permutation @var{p}.")

(defun-gsl permutation-inverse (inv p)
  "gsl_permutation_inverse"
  (((pointer inv) gsl-permutation-c) ((pointer p) gsl-permutation-c))
  :invalidate (inv)
  :documentation
  "Find the inverse of the permutation @var{p}.")

(defun-gsl permutation-next (p)
  "gsl_permutation_next"
  (((pointer p) gsl-permutation-c))
  :c-return :success-failure
  :invalidate (p)
  :documentation
  "Advance the permutation @var{p} to the next permutation
   in lexicographic order and return p and T.  If no further
   permutations are available, return p and NIL with
   @var{p} unmodified.  Starting with the identity permutation and
   repeatedly applying this function will iterate through all possible
   permutations of a given order.")

(defun-gsl permutation-previous (p)
  "gsl_permutation_prev"
  (((pointer p) gsl-permutation-c))
  :c-return :success-failure
  :invalidate (p)
  :documentation
  "Step backwards from the permutation @var{p} to the
   previous permutation in lexicographic order, returning p and T.
   If no previous permutation is available, return
   p and NIL with @var{p} unmodified.")

;;;;****************************************************************************
;;;; Applying Permutations
;;;;****************************************************************************

(defun-gsl permute (p data stride n)
  "gsl_permute"
  (((pointer p) gsl-permutation-c) (data :pointer) (stride :size) (n :size))
  :documentation
  "Apply the permutation @var{p} to the array @var{data} of
   size @var{n} with stride @var{stride}.")

(defun-gsl permute-inverse (p data stride n)
    "gsl_permute_inverse"
  (((pointer p) gsl-permutation-c) (data :pointer) (stride :size) (n :size))
  :documentation
  "Apply the inverse of the permutation @var{p} to the array @var{data} of
   size @var{n} with stride @var{stride}.")

(defun-gsl permute-vector (p v)
  "gsl_permute_vector"
  (((pointer p) gsl-permutation-c) ((pointer v) gsl-vector-c))
  :invalidate (v)
  :documentation
  "Apply the permutation @var{p} to the elements of the
   vector @var{v}, considered as a row-vector acted on by a permutation
   matrix from the right, @math{v' = v P}.  The @math{j}-th column of the
   permutation matrix @math{P} is given by the @math{p_j}-th column of the
   identity matrix. The permutation @var{p} and the vector @var{v} must
   have the same length.")

(defun-gsl permute-vector-inverse (p v)
  "gsl_permute_vector_inverse"
  (((pointer p) gsl-permutation-c) ((pointer v) gsl-vector-c))
  :invalidate (v)
  :documentation
  "Apply the inverse of the permutation @var{p} to the
  elements of the vector @var{v}, considered as a row-vector acted on by
  an inverse permutation matrix from the right, @math{v' = v P^T}.  Note
  that for permutation matrices the inverse is the same as the transpose.
  The @math{j}-th column of the permutation matrix @math{P} is given by
  the @math{p_j}-th column of the identity matrix. The permutation @var{p}
  and the vector @var{v} must have the same length.")

(defun-gsl permutation* (p pa pb)
  "gsl_permutation_mul"
  (((pointer p) gsl-permutation-c)
   ((pointer pa) gsl-permutation-c)
   ((pointer pb) gsl-permutation-c))
  :invalidate (p)
  :documentation
  "Combine the two permutations @var{pa} and @var{pb} into a
  single permutation @var{p}, where @math{p = pa . pb}. The permutation
  @var{p} is equivalent to applying @math{pb} first and then @var{pa}.")

;;;;****************************************************************************
;;;; Permutations in cyclic form
;;;;****************************************************************************

(defun-gsl linear-to-canonical (q p)
  "gsl_permutation_linear_to_canonical"
  (((pointer q) gsl-permutation-c) ((pointer p) gsl-permutation-c))
  :invalidate (q)
  :documentation
  "Compute the canonical form of the permutation @var{p} and
   stores it in the output argument @var{q}.")

(defun-gsl canonical-to-linear (p q)
  "gsl_permutation_canonical_to_linear"
  (((pointer p) gsl-permutation-c) ((pointer q) gsl-permutation-c))
  :invalidate (p)
  :documentation
  "Convert a permutation @var{q} in canonical form back into
   linear form storing it in the output argument @var{p}.")

(defun-gsl inversions (p)
  "gsl_permutation_inversions" (((pointer p) gsl-permutation-c))
  :c-return :size
  :documentation
  "Count the number of inversions in the permutation
  @var{p}.  An inversion is any pair of elements that are not in order.
  For example, the permutation 2031 has three inversions, corresponding to
  the pairs (2,0) (2,1) and (3,1).  The identity permutation has no
  inversions.")

(defun-gsl linear-cycles (p)
  "gsl_permutation_linear_cycles" (((pointer p) gsl-permutation-c))
  :c-return :size
  :documentation
  "Count the number of cycles in the permutation @var{p}, given in linear form.")

(defun-gsl canonical-cycles (p)
  "gsl_permutation_canonical_cycles"
  (((pointer p) gsl-permutation-c))
  :c-return :size
  :documentation
  "Count the number of cycles in the permutation @var{q},
   given in canonical form.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(defparameter *perm-1* (make-data 'permutation t 4))
(defparameter *perm-2* (make-data 'permutation t 4))

(lisp-unit:define-test permutation
  (lisp-unit:assert-eql			;gsl-aref
   2
   (progn
     (set-identity *perm-1*)
     (gsl-aref *perm-1* 2)))
  (lisp-unit:assert-equalp		;data
   #(0 1 2 3)
   (progn
     (set-identity *perm-1*)
     (data *perm-1*)))
  (lisp-unit:assert-equalp		;permutation-reverse
   #(3 2 1 0)
   (progn
     (set-identity *perm-1*)
     (data (permutation-reverse *perm-1*))))
  (lisp-unit:assert-equalp	;permutation-next, permutation-inverse
   #(0 3 1 2)
   (progn
     (set-identity *perm-1*)
     (permutation-next *perm-1*)
     (permutation-next *perm-1*)
     (permutation-next *perm-1*)
     (permutation-inverse *perm-2* *perm-1*)
     (data *perm-2*)))
  (lisp-unit:assert-equalp		;swap-elements
   #(0 3 2 1)
   (progn
     (set-identity *perm-1*)
     (swap-elements *perm-1* 1 3)
     (data *perm-1*)))
  (lisp-unit:assert-equalp		;permute-vector
   #(33 44 11 22)
   (progn
     (set-identity *perm-1*)
     (swap-elements *perm-1* 1 3)
     (swap-elements *perm-1* 0 2)
     (setf (data *intvec-1*) #(11 22 33 44)) 
     (permute-vector *perm-1* *intvec-1*)
     (data *intvec-1*)))
  (lisp-unit:assert-eql			;inversions
   3
   (progn
     (set-identity *perm-1*)
     (swap-elements *perm-1* 1 3)
     (inversions *perm-1*)))
  (lisp-unit:assert-eql			;linear-cycles
   3
   (progn
     (set-identity *perm-1*)
     (swap-elements *perm-1* 1 3)
     (linear-cycles *perm-1*)))
  (lisp-unit:assert-eql			;canonical-cycles
   2
   (progn
     (set-identity *perm-1*)
     (swap-elements *perm-1* 1 3)
     (swap-elements *perm-1* 0 2)
     (canonical-cycles *perm-1*))))

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************


#|
(with-data (perm permutation 4 t)
  (loop collect (data perm 'list)
    while (permutation-next perm)))

((0 1 2 3) (0 1 3 2) (0 2 1 3) (0 2 3 1) (0 3 1 2) (0 3 2 1) (1 0 2 3)
 (1 0 3 2) (1 2 0 3) (1 2 3 0) (1 3 0 2) (1 3 2 0) (2 0 1 3) (2 0 3 1)
 (2 1 0 3) (2 1 3 0) (2 3 0 1) (2 3 1 0) (3 0 1 2) (3 0 2 1) (3 1 0 2)
 (3 1 2 0) (3 2 0 1) (3 2 1 0))
|#
