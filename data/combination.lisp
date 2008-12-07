;; Combinations
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2008-12-06 18:57:42EST combination.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Combination structure and CL object
;;;;****************************************************************************

;;; GSL-combination definition
(cffi:defcstruct gsl-combination-c
  (n sizet)
  (k sizet)
  (data :pointer))

(defclass combination
    (#+sizet-64 vector-unsigned-byte-64
     #+sizet-32 vector-unsigned-byte-32)
  ((choice-of :initarg :choice-of :reader choice-of :type integer
	      :documentation "Maximum possible value; n in the (n k) notation."))
  (:documentation "GSL permutations."))

(export 'make-combination)

(defun make-combination (n &optional k (initialize t))
  "Make the object representing a combination of k things from a set of n.
   If initialize is T, initialize as the first k values (init-first).
   If n is a combination, make a new combination with the same
   specification.  If initialize is also T, copy it."
  (let ((comb
	 (if (typep n 'combination)
	     (make-instance
	      'combination :choice-of (choice-of n) :dimensions (dimensions k))
	     (make-instance 'combination :choice-of n :dimensions k))))
    (when initialize
      (if (typep n 'combination)
	  (copy comb n)
	  (init-first comb)))
    comb))

(defmethod alloc-gsl-struct ((object combination))
  (unless (and (slot-boundp object 'mpointer) (slot-value object 'mpointer))
    (let ((blockptr (cffi:foreign-alloc 'gsl-combination-c)))
      (setf (block-pointer object)
	    blockptr
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'data)
	    (c-pointer object)
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'n)
	    (choice-of object)
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'k)
	    (first (dimensions object))
	    (slot-value object 'mpointer)
	    (block-pointer object))
      (tg:finalize
       object
       (lambda () (cffi:foreign-free blockptr))))))

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defmfun init-first (combination)
  "gsl_combination_init_first"
  (((mpointer combination) :pointer))
  :c-return :void
  :inputs (combination)
  :outputs (combination)
  :documentation			; FDL
  "Initialize the combination c to the lexicographically
      first combination, i.e.  (0,1,2,...,k-1).")

(defmfun init-last (combination)
  "gsl_combination_init_last"
  (((mpointer combination) :pointer))
  :c-return :void
  :inputs (combination)
  :outputs (combination)
  :documentation			; FDL
  "Initialize the combination c to the lexicographically
   last combination, i.e. (n-k,n-k+1,...,n-1).")

(defmfun copy (destination source)
  "gsl_combination_memcpy"
  (((mpointer destination) :pointer)
   ((mpointer source) :pointer))
  :definition :method
  :inputs (source)
  :outputs (destination)
  :documentation			; FDL
  "Copy the elements of the combination source into the
  combination destination.  The two combinations must have the same size.")

;;;;****************************************************************************
;;;; Combination properties
;;;;****************************************************************************

(defmfun combination-range (c)
  "gsl_combination_n"
  (((mpointer c) :pointer))
  :c-return sizet
  :inputs (c)
  :documentation			; FDL
  "The range (n) of the combination c.")

(defmfun combination-size (c)
  "gsl_combination_k"
  (((mpointer c) :pointer))
  :c-return sizet
  :inputs (c)
  :documentation			; FDL
  "The number of elements (k) in the combination c.")

#|
;;; Unnecessary, gsl-array serves this function.
(defmfun combination-data (c)
  "gsl_combination_data"
  (((mpointer c) :pointer))
  :c-return :pointer
  :documentation			; FDL
  "A pointer to the array of elements in the combination.")
|#

(defmfun data-valid ((combination combination))
  "gsl_combination_valid"
  (((mpointer combination) :pointer))
  :definition :method 
  :c-return :boolean
  :documentation			; FDL
  "Check that the combination is valid.  The k
   elements should lie in the range 0 to n-1, with each
   value occurring once at most and in increasing order.")

;;;;****************************************************************************
;;;; Combination functions
;;;;****************************************************************************

(defmfun combination-next (c)
  "gsl_combination_next" (((mpointer c) :pointer))
  :c-return :success-failure
  :inputs (c)
  :outputs (c)
  :documentation			; FDL
  "Advance the combination c to the next combination
   in lexicographic order and return T and c.  If no further
   combinations are available it return NIL and c with
   c unmodified.  Starting with the first combination and
   repeatedly applying this function will iterate through all possible
   combinations of a given order.")

(defmfun combination-previous (c)
  "gsl_combination_prev"
  (((mpointer c) :pointer))
  :c-return :success-failure
  :inputs (c)
  :outputs (c)
  :documentation			; FDL
  "Step backwards from the combination c to the
   previous combination in lexicographic order, returning
   T and c.  If no previous combination is available it returns
   NIL and c with c unmodified.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test combination
 (let ((comb (make-combination 4 2)))	; combination-range
   (combination-range comb))
 (let ((comb (make-combination 4 2)))	; combination-size
   (combination-size comb))
 (let ((comb (make-combination 4 2)))	; init-first, combination-next
   (init-first comb)
   (loop collect (copy-seq (cl-array comb))
	 while (combination-next comb)))
 (let ((comb (make-combination 4 2)))  ; init-last, combination-previous
   (init-last comb)
   (loop collect (copy-seq (cl-array comb))
	 while (combination-previous comb)))
 (loop for i from 0 to 4		; combination-next
       append
       (let ((comb (make-combination 4 i)))
	 (init-first comb)
	 (loop collect (copy-seq (cl-array comb))
	       while (combination-next comb)))))
