;; Sorting
;; Liam Healy, Fri Apr 14 2006 - 20:20
;; Time-stamp: <2008-12-26 12:25:09EST sorting.lisp>
;; $Id$

(in-package :gsl)

;;; #'heapsort has just a cursory port, use CL's #'sort.

;;;;****************************************************************************
;;;; Heapsort, not recommended
;;;;****************************************************************************

(defmacro defcomparison (name &body body)
  `(cffi:defcallback ,name :int ((a :pointer) (b :pointer))
    ,body))

(defmfun heapsort (array count size function)
  "gsl_heapsort"
  ((array :pointer) (count sizet) (size sizet) (function :pointer))
  :documentation			; FDL
  "Sort the count elements of the array of size specified
   into ascending order using the comparison
   function.  The type of the comparison function is defined by,
   A comparison function should return a negative integer if the first
   argument is less than the second argument, zero if the two arguments
   are equal and a positive integer if the first argument is greater than
   the second argument."
  :c-return :void)

(defmfun heapsort-index (p array count size function)
  "gsl_heapsort_index"
  ((p sizet) (array :pointer) (count sizet) (size sizet) (function :pointer))
  :documentation			; FDL
  "Indirectly sort the count elements of the array
   array, each of size given, into ascending order using the
   comparison function.  The resulting permutation is stored
   in p, an array of length n.  The elements of p give the
   index of the array element which would have been stored in that position
   if the array had been sorted in place.  The first element of p
   gives the index of the least element in array, and the last
   element of p gives the index of the greatest element in
   array.  The array itself is not changed.")

;;;;****************************************************************************
;;;; Sorting vectors
;;;;****************************************************************************

;;; The GSL vector sorting routines come in two forms, those that sort
;;; on GSL vectors (with "vector" in the name), and those that sort on
;;; C arrays.  The former can sort vectors, the latter can both
;;; vectors and matrices.  The functions #'sort-index,
;;; #'sort-smallest-index, #'sort-largest-index could be made to work
;;; on matrices, but the indexing would have to be worked out
;;; correctly.

;;; It ought to be possible to provide a stride argument, but this
;;; gives an error:
;;;(defmfun msort ((v vector) &optional (stride 1))

(defmfun msort ((v both))
  ("gsl_sort" :type)
  (((c-pointer v) :pointer) (1 sizet) ((total-size v) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (v)
  :documentation			; FDL
  "Sort the n elements of the array data with stride stride into
   ascending numerical order.")

(defmfun sort-vector ((v vector))
  ("gsl_sort_vector" :type)
  (((mpointer v) :pointer))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (v)
  :documentation			; FDL
  "Sort the elements of the vector v into ascending numerical order.")

(defmfun sort-index ((permutation permutation) (vector vector))
  ("gsl_sort" :type "_index")
  (((mpointer permutation) :pointer)
   ((c-pointer vector) :pointer)
   (1 sizet) ((dim0 vector) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (vector)
  :outputs (permutation)
  :documentation			; FDL
  "Indirectly sort the n elements of the array vector with stride stride
   into ascending order, storing the resulting permutation.  The latter
   must be created with the same size as the vector.
   The elements of permutation give the index of the
   array element which would have been stored in that position if the
   array had been sorted in place. The array data is not changed.")

(defmfun sort-vector-index ((permutation permutation) (vector vector))
  ("gsl_sort_vector" :type "_index")
  (((mpointer permutation) :pointer)
   ((mpointer vector) :pointer))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (vector)
  :outputs (permutation)
  :documentation			; FDL
  "Indirectly sort the elements of the vector v into
   ascending order, storing the resulting permutation in p.  The
   elements of p give the index of the vector element which would
   have been stored in that position if the vector had been sorted in
   place.  The first element of p gives the index of the least element
   in v and the last element of p gives the index of the
   greatest element in v.  The vector v is not changed.")

;;;;****************************************************************************
;;;; Selecting the k smallest or largest elements
;;;;****************************************************************************

(defmfun sort-vector-smallest (dest (v vector))
  ("gsl_sort_vector" :type "_smallest")
  (((c-pointer dest) :pointer) ((dim0 dest) sizet)
   ((mpointer v) :pointer))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (dest)
  :documentation			; FDL
  "Find the smallest elements of the vector v and put them into dest,
   which must be shorter than v.")

(defmfun sort-smallest (dest (v both))
  ("gsl_sort" :type "_smallest")
  (((c-pointer dest) :pointer) ((total-size dest) sizet)
   ((c-pointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((total-size v) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (dest)
  :documentation			; FDL
  "Find the smallest elements of the vector v and put them into dest,
   which must be shorter than v.")

(defmfun sort-vector-smallest-index (combination (v vector))
  ("gsl_sort_vector" :type "_smallest_index")
  (((c-pointer combination) :pointer) ((total-size combination) sizet)
   ((mpointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((first (dimensions combination)) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (combination)
  :documentation
  "The indices of the smallest elements of the vector stored,
    returned as a CL vector of element type fixnum.  If
    indices is a positive initeger, a vector will be
    allocated and returned.  If it is a CL vector,
    it will be filled with the indices.")

(defmfun sort-smallest-index (combination (v vector))
  ("gsl_sort" :type "_smallest_index")
  (((c-pointer combination) :pointer) ((total-size combination) sizet)
   ((c-pointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((first (dimensions combination)) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (combination)
  :documentation
  "The indices of the smallest elements of the vector stored,
    returned as a CL vector of element type fixnum.  If
    indices is a positive initeger, a vector will be
    allocated and returned.  If it is a CL vector,
    it will be filled with the indices.")

(defmfun sort-vector-largest (dest (v vector))
  ("gsl_sort_vector" :type "_largest")
  (((c-pointer dest) :pointer) ((dim0 dest) sizet)
   ((mpointer v) :pointer))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (dest)
  :documentation			; FDL
  "Find the largest elements of the vector v and put them into dest,
   which must be shorter than v.")

(defmfun sort-largest (dest (v both))
  ("gsl_sort" :type "_largest")
  (((c-pointer dest) :pointer) ((total-size dest) sizet)
   ((c-pointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((total-size v) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (dest)
  :documentation			; FDL
  "Find the largest elements of the vector v and put them into dest,
   which must be shorter than v.")

(defmfun sort-vector-largest-index (combination (v vector))
  ("gsl_sort_vector" :type "_largest_index")
  (((c-pointer combination) :pointer) ((total-size combination) sizet)
   ((mpointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((first (dimensions combination)) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :inputs (v)
  :outputs (combination)
  :documentation
  "The indices of the largest elements of the vector stored,
    returned as a CL vector of element type fixnum.  If
    indices is a positive initeger, a vector will be
    allocated and returned.  If it is a CL vector,
    it will be filled with the indices.")

(defmfun sort-largest-index (combination (v vector))
  ("gsl_sort" :type "_largest_index")
  (((c-pointer combination) :pointer) ((total-size combination) sizet)
   ((c-pointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((first (dimensions combination)) sizet))
  :element-types :no-complex
  :definition :generic
  :c-return :void
  :inputs (v)
  :outputs (combination)
  :documentation
  "The indices of the largest elements of the vector stored,
    returned as a CL vector of element type fixnum.  If
    indices is a positive initeger, a vector will be
    allocated and returned.  If it is a CL vector,
    it will be filled with the indices.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(generate-all-array-tests sort-vector :no-complex
 (let ((v1 (array-default 8)))
   ;; or you can use msort
   (cl-array (sort-vector v1))))

(generate-all-array-tests sort-matrix :no-complex
 (let ((m1 (array-default '(3 3))))
   (cl-array (msort m1))))

(generate-all-array-tests sort-vector-index :no-complex
 (let ((perm (make-permutation 8))
	(v1 (array-default 8)))
   (sort-vector-index perm v1)
   (cl-array perm)))

(generate-all-array-tests sort-vector-smallest :no-complex
 (let ((v1 (array-default 8))
	(v2 (array-default 3)))
   (cl-array (sort-vector-smallest v2 v1))))

(generate-all-array-tests sort-matrix-smallest :no-complex
 (let ((m1 (array-default '(3 3)))
	(m2 (array-default '(2 3) t)))
   (cl-array (sort-smallest m2 m1))))

(generate-all-array-tests sort-vector-smallest-index :no-complex
 (let ((comb (make-combination '(8 3)))
	(v1 (array-default 8)))
   (cl-array (sort-vector-smallest-index comb v1))))

(generate-all-array-tests sort-vector-largest :no-complex
 (let ((v1 (array-default 8))
	(v2 (array-default 3)))
   (cl-array (sort-vector-largest v2 v1))))

(generate-all-array-tests sort-matrix-largest :no-complex
 (let ((m1 (array-default '(3 3)))
	(m2 (array-default '(2 3) t)))
   (cl-array (sort-largest m2 m1))))

(generate-all-array-tests sort-vector-largest-index :no-complex
 (let ((comb (make-combination '(8 3)))
	(v1 (array-default 8)))
   (cl-array (sort-vector-largest-index comb v1))))
