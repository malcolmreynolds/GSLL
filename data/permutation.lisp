;; Permutations
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2008-02-23 18:49:22EST permutation.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Permutation structure and CL object
;;;;****************************************************************************

;;; GSL-permutation definition
(cffi:defcstruct gsl-permutation-c
  (size size)
  (data :pointer))

;;; Allocation, freeing, reading and writing
(defdata "permutation" permutation unsigned-byte)

(defmethod gsl-array ((object gsl-permutation))
  (foreign-slot-value (pointer object) 'gsl-permutation-c 'data))

;;;;****************************************************************************
;;;; Getting values
;;;;****************************************************************************

(defmfun maref ((permutation gsl-permutation) &rest indices)
  "gsl_permutation_get"
  (((pointer permutation) :pointer) ((first indices) size))
  :type :method 
  :c-return size
  :documentation "The ith element of the permutation.")	; FDL

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defmfun set-identity ((permutation gsl-permutation))
  "gsl_permutation_init"
  (((pointer permutation) :pointer))
  :type :method
  :c-return :void
  :documentation			; FDL
  "Initialize the permutation p to the identity, i.e.
   (0,1,2,...,n-1).")

(defmfun copy ((destination gsl-permutation) (source gsl-permutation))
  "gsl_permutation_memcpy"
  (((pointer destination) gsl-permutation-c)
   ((pointer source) gsl-permutation-c))
  :type :method
  :invalidate (destination)
  :documentation			; FDL
  "Copy the elements of the permutation source into the
   permutation destination.  The two permutations must have the same size.")

(defmfun swap-elements ((p gsl-permutation) i j)
  "gsl_permutation_swap"
  (((pointer p) gsl-permutation-c) (i size) (j size))
  :type :method
  :invalidate (p)
  :documentation			; FDL
  "Exchanges the ith and jth elements of the permutation p.")

;;;;****************************************************************************
;;;; Permutation properties
;;;;****************************************************************************

(defmfun permutation-size (p)
  "gsl_permutation_size"
  (((pointer p) gsl-permutation-c))
  :c-return size
  :documentation			; FDL
  "The size of the permutation p.")

(defmfun permutation-data (p)
  "gsl_permutation_data"
  (((pointer p) gsl-permutation-c))
  :c-return :pointer
  :documentation			; FDL
  "A pointer to the array of elements in the
   permutation p.")

(defmfun data-valid ((permutation gsl-permutation))
  "gsl_permutation_valid"
  (((pointer permutation) :pointer))
  :type :method 
  :c-return :boolean
  :documentation			; FDL
  "Check that the permutation p is valid.  The n
  elements should contain each of the numbers 0 to n-1 once and only
  once.")

;;;;****************************************************************************
;;;; Permutation functions
;;;;****************************************************************************

(defmfun permutation-reverse (p)
  "gsl_permutation_reverse"
  (((pointer p) gsl-permutation-c))
  :invalidate (p)
  :c-return :void
  :documentation			; FDL
  "Reverse the order of the elements of the permutation p.")

(defmfun permutation-inverse (inv p)
  "gsl_permutation_inverse"
  (((pointer inv) gsl-permutation-c) ((pointer p) gsl-permutation-c))
  :invalidate (inv)
  :documentation			; FDL
  "Find the inverse of the permutation p.")

(defmfun permutation-next (p)
  "gsl_permutation_next"
  (((pointer p) gsl-permutation-c))
  :c-return :success-failure
  :invalidate (p)
  :documentation			; FDL
  "Advance the permutation p to the next permutation
   in lexicographic order and return p and T.  If no further
   permutations are available, return p and NIL with
   p unmodified.  Starting with the identity permutation and
   repeatedly applying this function will iterate through all possible
   permutations of a given order.")

(defmfun permutation-previous (p)
  "gsl_permutation_prev"
  (((pointer p) gsl-permutation-c))
  :c-return :success-failure
  :invalidate (p)
  :documentation			; FDL
  "Step backwards from the permutation p to the
   previous permutation in lexicographic order, returning p and T.
   If no previous permutation is available, return
   p and NIL with p unmodified.")

;;;;****************************************************************************
;;;; Applying Permutations
;;;;****************************************************************************

(defmfun permute (p data stride n)
  "gsl_permute"
  (((pointer p) gsl-permutation-c) (data :pointer) (stride size) (n size))
  :documentation			; FDL
  "Apply the permutation p to the array data of
   size n with stride stride.")

(defmfun permute-inverse (p data stride n)
    "gsl_permute_inverse"
  (((pointer p) gsl-permutation-c) (data :pointer) (stride size) (n size))
  :documentation			; FDL
  "Apply the inverse of the permutation p to the array data of
   size n with stride.")

(export 'permute-vector)
(defgeneric permute-vector (permutation vector)
  (:documentation			; FDL
  "Apply the permutation p to the elements of the
   vector v considered as a row-vector acted on by a permutation
   matrix from the right, v' = v P.  The jth column of the
   permutation matrix P is given by the p_j-th column of the
   identity matrix. The permutation p and the vector v must
   have the same length."))

(defmfun-vdsfc permute-vector (p (v gsl-vector))
  "gsl_permute_vector"
  (((pointer p) gsl-permutation-c) ((pointer v) gsl-vector-c))
  :invalidate (v))

(export 'permute-vector-inverse)
(defgeneric permute-vector-inverse (permutation vector)
  (:documentation			; FDL
  "Apply the inverse of the permutation p to the
  elements of the vector v considered as a row-vector acted on by
  an inverse permutation matrix from the right, v' = v P^T.  Note
  that for permutation matrices the inverse is the same as the transpose.
  The jth column of the permutation matrix P is given by
  the p_j-th column of the identity matrix. The permutation p
  and the vector v must have the same length."))

(defmfun-vdsfc permute-vector-inverse (p (v gsl-vector))
  "gsl_permute_vector_inverse"
  (((pointer p) gsl-permutation-c) ((pointer v) gsl-vector-c))
  :invalidate (v))

(defmfun permutation* (p pa pb)
  "gsl_permutation_mul"
  (((pointer p) gsl-permutation-c)
   ((pointer pa) gsl-permutation-c)
   ((pointer pb) gsl-permutation-c))
  :invalidate (p)
  :documentation			; FDL
  "Combine the two permutations pa and pb into a
  single permutation p where p = pa . pb. The permutation
  p is equivalent to applying pb first and then pa.")

;;;;****************************************************************************
;;;; Permutations in cyclic form
;;;;****************************************************************************

(defmfun linear-to-canonical (q p)
  "gsl_permutation_linear_to_canonical"
  (((pointer q) gsl-permutation-c) ((pointer p) gsl-permutation-c))
  :invalidate (q)
  :documentation			; FDL
  "Compute the canonical form of the permutation p and
   stores it in the output argument q.")

(defmfun canonical-to-linear (p q)
  "gsl_permutation_canonical_to_linear"
  (((pointer p) gsl-permutation-c) ((pointer q) gsl-permutation-c))
  :invalidate (p)
  :documentation			; FDL
  "Convert a permutation q in canonical form back into
   linear form storing it in the output argument p.")

(defmfun inversions (p)
  "gsl_permutation_inversions" (((pointer p) gsl-permutation-c))
  :c-return size
  :documentation			; FDL
  "Count the number of inversions in the permutation
  p.  An inversion is any pair of elements that are not in order.
  For example, the permutation 2031 has three inversions, corresponding to
  the pairs (2,0) (2,1) and (3,1).  The identity permutation has no
  inversions.")

(defmfun linear-cycles (p)
  "gsl_permutation_linear_cycles" (((pointer p) gsl-permutation-c))
  :c-return size
  :documentation			; FDL
  "Count the number of cycles in the permutation p, given in linear form.")

(defmfun canonical-cycles (p)
  "gsl_permutation_canonical_cycles"
  (((pointer p) gsl-permutation-c))
  :c-return size
  :documentation			; FDL
  "Count the number of cycles in the permutation q, given in canonical form.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(defun generate-all-permutations (n)
  "Generate all the permutations of n objects."
  (letm ((perm (permutation n t)))
    (loop collect (data perm 'list)
	  while (permutation-next perm))))

#|
(make-tests
 permutation
 (letm ((perm-1 (permutation 4 t)))	;maref
   (set-identity perm-1)
   (maref perm-1 2))
 (letm ((perm-1 (permutation 4 t)))	;data
   (set-identity perm-1)
   (data perm-1))
 (letm ((perm-1 (permutation 4 t)))	;permutation-reverse
   (set-identity perm-1)
   (data (permutation-reverse perm-1)))
 (letm				;permutation-next, permutation-inverse
     ((perm-1 (permutation 4 t)) (perm-2 (permutation 4 t)))
   (set-identity perm-1)
   (permutation-next perm-1)
   (permutation-next perm-1)
   (permutation-next perm-1)
   (permutation-inverse perm-2 perm-1)
   (data perm-2))
 (letm ((perm-1 (permutation 4 t)))	;swap-elements
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (data perm-1))
 (letm ((perm-1 (permutation 4 t))	;permute-vector
	(intvec (vector-fixnum #(11 22 33 44))))
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (swap-elements perm-1 0 2)
   (permute-vector perm-1 intvec)
   (data intvec))
 (letm ((perm-1 (permutation 4 t)))	;inversions
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (inversions perm-1))
 (letm ((perm-1 (permutation 4 t)))	;linear-cycles
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (linear-cycles perm-1))
 (letm ((perm-1 (permutation 4 t)))	;canonical-cycles
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (swap-elements perm-1 0 2)
   (canonical-cycles perm-1)))
|#

(LISP-UNIT:DEFINE-TEST PERMUTATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2)
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T)))
      (SET-IDENTITY PERM-1)
      (MAREF PERM-1 2))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(0 1 2 3))
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T)))
      (SET-IDENTITY PERM-1)
      (DATA PERM-1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(3 2 1 0))
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T)))
      (SET-IDENTITY
       PERM-1)
      (DATA (PERMUTATION-REVERSE PERM-1)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(0 3 1 2))
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T))
	 (PERM-2 (PERMUTATION 4 T)))
      (SET-IDENTITY PERM-1)
      (PERMUTATION-NEXT PERM-1)
      (PERMUTATION-NEXT PERM-1)
      (PERMUTATION-NEXT PERM-1)
      (PERMUTATION-INVERSE PERM-2 PERM-1)
      (DATA PERM-2))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(0 3 2 1))
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T)))
      (SET-IDENTITY PERM-1)
      (SWAP-ELEMENTS PERM-1 1 3)
      (DATA PERM-1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(33 44 11 22))
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T))
	 (INTVEC (VECTOR-FIXNUM
	   #(11 22 33 44))))
      (SET-IDENTITY PERM-1)
      (SWAP-ELEMENTS PERM-1 1 3)
      (SWAP-ELEMENTS PERM-1 0 2)
      (PERMUTE-VECTOR PERM-1 INTVEC)
      (DATA INTVEC))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3)
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T)))
      (SET-IDENTITY PERM-1)
      (SWAP-ELEMENTS PERM-1 1 3)
      (INVERSIONS PERM-1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3)
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T)))
      (SET-IDENTITY PERM-1)
      (SWAP-ELEMENTS PERM-1 1 3)
      (LINEAR-CYCLES PERM-1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2)
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM-1 (PERMUTATION 4 T)))
      (SET-IDENTITY PERM-1)
      (SWAP-ELEMENTS PERM-1 1 3)
      (SWAP-ELEMENTS PERM-1 0 2)
      (CANONICAL-CYCLES PERM-1)))))

