;; Combinations
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2008-12-06 14:10:53EST combination.lisp>
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
  ()
  (:documentation "GSL permutations."))

(export 'make-combination)
(defun make-combination (nk)
  "Make the combination object with the data array."
  (let ((k (second nk)))
    (make-instance
     'combination
     :cl-array (make-array* k *sizet-type*)
     :mpointer nil	   ; this will be set by :before method below.
     #-native :c-pointer #-native nil	; this will be set by defmfun
     :dimensions (copy-list nk)
     ;; The total-size of a combination is k, because that is the length
     ;; of the vector that represents it.
     :total-size k)))

(defmethod alloc-gsl-struct ((object combination))
  (unless (slot-value object 'mpointer)
    (let ((blockptr (cffi:foreign-alloc 'gsl-combination-c)))
      (setf (block-pointer object)
	    blockptr
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'data)
	    (c-pointer object)
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'n)
	    (elt (dimensions object) 0)
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'k)
	    (elt (dimensions object) 1)
	    (mpointer object)
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
 (letm ((comb (combination '(4 2) t)))	; combination-range
   (combination-range comb))
 (letm ((comb (combination '(4 2) t)))	; combination-size
   (combination-size comb))
 (letm ((comb (combination '(4 2) t)))	; init-first, combination-next
   (init-first comb)
   (loop collect (copy-seq (cl-array comb))
	 while (combination-next comb)))
 (letm ((comb (combination '(4 2) t)))  ; init-last, combination-previous
   (init-last comb)
   (loop collect (copy-seq (cl-array comb))
	 while (combination-previous comb)))
 (loop for i from 0 to 4		; combination-next
       append
       (letm ((comb (combination (list 4 i) t)))
	 (init-first comb)
	 (loop collect (copy-seq (cl-array comb))
	       while (combination-next comb)))))
