;; Permutations
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2009-03-17 20:27:01EDT permutation.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Permutation structure and CL object
;;;;****************************************************************************
   
(defclass permutation (mobject foreign-array)
  ((element-type
    :initform
    #+int64 '(unsigned-byte 64)
    #+int32 '(unsigned-byte 32)
    :reader element-type :allocation :class))
  (:documentation "GSL permutations."))

(cffi:defcstruct gsl-permutation-c	; The GSL struct
  (size sizet)
  (data :pointer))

(defmethod initialize-instance :after
    ((object permutation) &key dimensions &allow-other-keys)
  (let ((mptr (cffi:foreign-alloc 'gsl-permutation-c)))
    (setf (slot-value object 'mpointer)
	  mptr
	  (cffi:foreign-slot-value mptr 'gsl-permutation-c 'data)
	  (c-pointer object)
	  (cffi:foreign-slot-value mptr 'gsl-permutation-c 'size)
	  dimensions)
    (tg:finalize object (lambda () (cffi:foreign-free mptr)))))

(export 'make-permutation)
(defun make-permutation (n &optional (initialize t))
  "Make the object representing a permutation of n objects.
   If n is a permutation, make a new permutation of the same size.  If
   initialize is T (default), set to the identity permutation if n is
   an integer, or copy the permutation if it's a permutation."
  (let ((perm
	 (make-instance
	  'permutation
	  :dimensions (if (typep n 'permutation) (dimensions n) n))))
    (when initialize
      (if (typep n 'permutation)
	  (copy perm n)
	  (set-identity perm)))
    perm))

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defmfun set-identity ((permutation permutation))
  "gsl_permutation_init"
  (((mpointer permutation) :pointer))
  :definition :method
  :c-return :void
  :outputs (permutation)
  :return (permutation)
  :documentation			; FDL
  "Initialize the permutation p to the identity, i.e.
   (0,1,2,...,n-1).")

(defmfun copy-to-destination ((source permutation) (destination permutation))
  "gsl_permutation_memcpy"
  (((mpointer destination) :pointer)
   ((mpointer source) :pointer))
  :definition :method
  :inputs (source)
  :outputs (destination)
  :return (destination)
  :index copy
  :documentation			; FDL
  "Copy the elements of the permutation source into the
   permutation destination.  The two permutations must have the same size.")

(defmfun swap-elements ((p permutation) i j)
  "gsl_permutation_swap"
  (((mpointer p) :pointer) (i sizet) (j sizet))
  :definition :method
  :inputs (p)
  :outputs (p)
  :return (p)
  :documentation			; FDL
  "Exchanges the ith and jth elements of the permutation p.")

;;;;****************************************************************************
;;;; Permutation properties
;;;;****************************************************************************

(defmfun permutation-size (p)
  "gsl_permutation_size"
  (((mpointer p) :pointer))
  :c-return sizet
  :inputs (p)
  :documentation			; FDL
  "The size of the permutation p.")

(defmfun permutation-data (p)
  "gsl_permutation_data"
  (((mpointer p) :pointer))
  :c-return :pointer
  :inputs (p)
  :documentation			; FDL
  "A pointer to the array of elements in the
   permutation p.")

(defgeneric validp (object)
  (:documentation			; FDL
   "Check that the object p is valid."))

(defmfun validp ((permutation permutation))
  "gsl_permutation_valid"
  (((mpointer permutation) :pointer))
  :definition :method
  :c-return :success-failure
  :inputs (permutation)
  :documentation			; FDL
  "Check that the permutation p is valid.  The n
  elements should contain each of the numbers 0 to n-1 once and only
  once.")

;;;;****************************************************************************
;;;; Permutation functions
;;;;****************************************************************************

(defmfun permutation-reverse (p)
  "gsl_permutation_reverse"
  (((mpointer p) :pointer))
  :c-return :void
  :inputs (p)
  :outputs (p)
  :return (p)
  :documentation			; FDL
  "Reverse the order of the elements of the permutation p.")

(defmfun permutation-inverse (inv p)
  "gsl_permutation_inverse"
  (((mpointer inv) :pointer) ((mpointer p) :pointer))
  :inputs (p)
  :outputs (inv)
  :return (inv)
  :documentation			; FDL
  "Find the inverse of the permutation p.")

(defmfun permutation-next (p)
  "gsl_permutation_next"
  (((mpointer p) :pointer))
  :c-return :success-failure
  :inputs (p)
  :outputs (p)
  :return (p :c-return)
  :documentation			; FDL
  "Advance the permutation p to the next permutation
   in lexicographic order and return p and T.  If no further
   permutations are available, return p and NIL with
   p unmodified.  Starting with the identity permutation and
   repeatedly applying this function will iterate through all possible
   permutations of a given order.")

(defmfun permutation-previous (p)
  "gsl_permutation_prev"
  (((mpointer p) :pointer))
  :c-return :success-failure
  :inputs (p)
  :outputs (p)
  :return (p :c-return)
  :documentation			; FDL
  "Step backwards from the permutation p to the
   previous permutation in lexicographic order, returning p and T.
   If no previous permutation is available, return
   p and NIL with p unmodified.")

;;;;****************************************************************************
;;;; Applying Permutations
;;;;****************************************************************************

(defmfun permute ((p permutation) (v vector) &optional size stride)
  ("gsl_permute_vector" :type)
  (((mpointer p) :pointer) ((mpointer v) :pointer))
  :definition :generic
  :inputs (p v)
  :outputs (v)
  :return (v)
  :documentation			; FDL
  "Apply the permutation p to the elements of the
   vector v considered as a row-vector acted on by a permutation
   matrix from the right, v' = v P.  The jth column of the
   permutation matrix P is given by the p_j-th column of the
   identity matrix. The permutation p and the vector v must
   have the same length.")

(defmfun permute
    (p (data #.+foreign-pointer-class+) &optional (size 1) (stride 1))
  "gsl_permute"
  (((mpointer p) :pointer) (data :pointer) (stride sizet) (size sizet))
  :definition :method
  :inputs (p data)
  :outputs (data)
  :return (data)
  :documentation			; FDL
  "Apply the permutation p to the array data of
   size n with stride stride.")

(defmfun permute-inverse
    ((p permutation) (v vector) &optional size stride)
  ("gsl_permute_vector" :type "_inverse")
  (((mpointer p) :pointer) ((mpointer v) :pointer))
  :definition :generic
  :inputs (p v)
  :outputs (v)
  :return (v)
  :documentation			; FDL
  "Apply the permutation p to the elements of the
   vector v considered as a row-vector acted on by a permutation
   matrix from the right, v' = v P.  The jth column of the
   permutation matrix P is given by the p_j-th column of the
   identity matrix. The permutation p and the vector v must
   have the same length.")

(defmfun permute-inverse
    (p (data #.+foreign-pointer-class+) &optional (size 1) (stride 1))
  "gsl_permute_inverse"
  (((mpointer p) :pointer) (data :pointer) (stride sizet) (n sizet))
  :definition :method
  :inputs (p data)
  :outputs (data)
  :return (data)
  :documentation			; FDL
  "Apply the inverse of the permutation p to the array data of
   size n with stride.")

(defmfun permutation* (p pa pb)
  "gsl_permutation_mul"
  (((mpointer p) :pointer)
   ((mpointer pa) :pointer)
   ((mpointer pb) :pointer))
  :inputs (pa pb)
  :outputs (p)
  :return (p)
  :documentation			; FDL
  "Combine the two permutations pa and pb into a
  single permutation p where p = pa . pb. The permutation
  p is equivalent to applying pb first and then pa.")

;;;;****************************************************************************
;;;; Permutations in cyclic form
;;;;****************************************************************************

(defmfun linear-to-canonical (q p)
  "gsl_permutation_linear_to_canonical"
  (((mpointer q) :pointer) ((mpointer p) :pointer))
  :inputs (p)
  :outputs (q)
  :documentation			; FDL
  "Compute the canonical form of the permutation p and
   stores it in the output argument q.")

(defmfun canonical-to-linear (p q)
  "gsl_permutation_canonical_to_linear"
  (((mpointer p) :pointer) ((mpointer q) :pointer))
  :inputs (q)
  :outputs (p)
  :documentation			; FDL
  "Convert a permutation q in canonical form back into
   linear form storing it in the output argument p.")

(defmfun inversions (p)
  "gsl_permutation_inversions" (((mpointer p) :pointer))
  :c-return sizet
  :inputs (p)
  :documentation			; FDL
  "Count the number of inversions in the permutation
  p.  An inversion is any pair of elements that are not in order.
  For example, the permutation 2031 has three inversions, corresponding to
  the pairs (2,0) (2,1) and (3,1).  The identity permutation has no
  inversions.")

(defmfun linear-cycles (p)
  "gsl_permutation_linear_cycles" (((mpointer p) :pointer))
  :c-return sizet
  :inputs (p)
  :documentation			; FDL
  "Count the number of cycles in the permutation p, given in linear form.")

(defmfun canonical-cycles (p)
  "gsl_permutation_canonical_cycles"
  (((mpointer p) :pointer))
  :c-return sizet
  :inputs (p)
  :documentation			; FDL
  "Count the number of cycles in the permutation q, given in canonical form.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(defun generate-all-permutations (n)
  "Generate all the permutations of n objects."
  (let ((perm (make-permutation n)))
    (set-identity perm)
    (loop collect (copy-seq (cl-array perm))
	  while (permutation-next perm))))

(defun generate-all-permutations-backwards (n)
  "Generate all the permutations of n objects."
  (let ((perm (make-permutation n)))
    (set-identity perm)
    (permutation-reverse perm)
    (loop collect (copy-seq (cl-array perm))
	  while (permutation-previous perm))))

(save-test permutation
 (let ((perm-1 (make-permutation 4 t)))	;maref
   (maref perm-1 2))
 (let ((perm-1 (make-permutation 4 t)))	;cl-array
   (cl-array perm-1))
 (let ((perm-1 (make-permutation 4 t)))	;permutation-reverse
   (set-identity perm-1)
   (cl-array (permutation-reverse perm-1)))
 (let				;permutation-next, permutation-inverse
     ((perm-1 (make-permutation 4 t)) (perm-2 (make-permutation 4 t)))
   (set-identity perm-1)
   (permutation-next perm-1)
   (permutation-next perm-1)
   (permutation-next perm-1)
   (permutation-inverse perm-2 perm-1)
   (cl-array perm-2))
 (let ((perm-1 (make-permutation 4 t)))	;swap-elements
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (cl-array perm-1))
 (let ((perm-1 (make-permutation 4 t))	;permute-vector
       (intvec #31m(11 22 33 44)))
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (swap-elements perm-1 0 2)
   (permute perm-1 intvec)
   (cl-array intvec))
 (let ((perm-1 (make-permutation 4 t)))	;inversions
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (inversions perm-1))
 (let ((perm-1 (make-permutation 4 t)))	;linear-cycles
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (linear-cycles perm-1))
 (let ((perm-1 (make-permutation 4 t)))	;canonical-cycles
   (set-identity perm-1)
   (swap-elements perm-1 1 3)
   (swap-elements perm-1 0 2)
   (canonical-cycles perm-1)))
