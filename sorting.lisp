;; Sorting
;; Liam Healy, Fri Apr 14 2006 - 20:20
;; Time-stamp: <2008-09-20 22:29:33EDT sorting.lisp>
;; $Id$

(in-package :gsl)

;;; #'heapsort has just a cursory port, use CL's #'sort.
;;; Raw C array functions not ported, not policy.

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
;;; C arrays.  From the point of view of GSLL, it makes little sense
;;; to have both, since we can always use the C form, and if we don't
;;; need the GSL vector, it won't be constructed.  However, it is
;;; trivial to make both forms, so they are both defined here.

;;; It ought to be possible to provide a stride argument, but this
;;; gives an error:
;;;(defmfun msort ((v vector) &optional (stride 1))


(defmfun msort ((v vector))
  ("gsl_sort" :type)
  (((c-pointer v) :pointer) (1 sizet) ((dim0 v) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
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
  :outputs (dest)
  :documentation			; FDL
  "Find the smallest elements of the vector v and put them into dest,
   which must be shorter than v.")

(defmfun sort-smallest (dest (v vector))
  ("gsl_sort" :type "_smallest")
  (((c-pointer dest) :pointer) ((dim0 dest) sizet)
   ((c-pointer v) :pointer))
  :definition :generic
  :element-types :no-complex
  :c-return :void
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
  :return (combination)
  :documentation
  "The indices of the smallest elements of the vector stored,
    returned as a CL vector of element type fixnum.  If
    indices is a positive initeger, a vector will be
    allocated and returned.  If it is a CL vector,
    it will be filled with the indices.")

(defmfun sort-smallest-index
    (combination (v vector))
  ("gsl_sort" :type "_smallest_index")
  (((c-pointer combination) :pointer) ((total-size combination) sizet)
   ((c-pointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((first (dimensions combination)) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :return (combination)
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
  :outputs (dest)
  :documentation			; FDL
  "Find the largest elements of the vector v and put them into dest,
   which must be shorter than v.")

(defmfun sort-largest (dest (v vector))
  ("gsl_sort" :type "_largest")
  (((c-pointer dest) :pointer) ((dim0 dest) sizet)
   ((c-pointer v) :pointer))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :outputs (dest)
  :documentation			; FDL
  "Find the largest elements of the vector v and put them into dest,
   which must be shorter than v.")

(defmfun sort-vector-largest-index
    (combination (v vector))
  ("gsl_sort_vector" :type "_largest_index")
  (((c-pointer combination) :pointer) ((total-size combination) sizet)
   ((mpointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((first (dimensions combination)) sizet))
  :definition :generic
  :element-types :no-complex
  :c-return :void
  :return (combination)
  :documentation
  "The indices of the largest elements of the vector stored,
    returned as a CL vector of element type fixnum.  If
    indices is a positive initeger, a vector will be
    allocated and returned.  If it is a CL vector,
    it will be filled with the indices.")

(defmfun sort-largest-index
    (combination (v vector))
  ("gsl_sort" :type "_largest_index")
  (((c-pointer combination) :pointer) ((total-size combination) sizet)
   ((c-pointer v) :pointer)
   (1 sizet)				; stride, set to 1 for now
   ((first (dimensions combination)) sizet))
  :element-types :no-complex
  :definition :generic
  :c-return :void
  :return (combination)
  :documentation
  "The indices of the largest elements of the vector stored,
    returned as a CL vector of element type fixnum.  If
    indices is a positive initeger, a vector will be
    allocated and returned.  If it is a CL vector,
    it will be filled with the indices.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests sorting
 (letm ((vec (vector-double-float (a 7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))))
   (sort-vector vec)
   (cl-array vec))
 (letm ((perm (permutation 5))
	(vec (vector-double-float (a 7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))))
   (sort-vector-index perm vec)
   (cl-array perm))
 (letm ((vec (vector-double-float (a 7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(smallest (vector-double-float 3)))
   (sort-vector-smallest smallest vec)
   (cl-array smallest))
 (letm ((vec (vector-double-float (a 7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(comb (combination '(5 3))))
   (cl-array (sort-vector-smallest-index comb vec)))
 (letm ((vec (vector-double-float (a 7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(sm (combination '(5 3))))
   (cl-array (sort-smallest-index sm vec)))
 (letm ((vec (vector-double-float (a 7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(largest (vector-double-float 3)))
   (sort-vector-largest largest vec)
   (cl-array largest))
 (letm ((vec (vector-double-float (a 7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(comb (combination '(5 3))))
   (cl-array (sort-vector-largest-index comb vec))))
|#


(LISP-UNIT:DEFINE-TEST SORTING
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(-3.21 -2.0 1.0 7.1 12.8))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE-FLOAT (A 7.1 -2.0 12.8 -3.21 1.0))))
      (SORT-VECTOR VEC) (CL-ARRAY VEC))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(3 1 4 0 2))
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM (PERMUTATION 5))
	   (VEC (VECTOR-DOUBLE-FLOAT (A 7.1 -2.0 12.8 -3.21 1.0))))
      (SORT-VECTOR-INDEX PERM VEC)
      (CL-ARRAY PERM))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(-3.21 -2.0 1.0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE-FLOAT (A 7.1 -2.0 12.8 -3.21 1.0)))
	   (SMALLEST (VECTOR-DOUBLE-FLOAT 3)))
      (SORT-VECTOR-SMALLEST SMALLEST VEC)
      (CL-ARRAY SMALLEST))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(3 1 4))
   (MULTIPLE-VALUE-LIST
    (LETM
	((VEC (VECTOR-DOUBLE-FLOAT
	       (A 7.1 -2.0 12.8 -3.21 1.0)))
	 (COMB (COMBINATION '(5 3))))
      (CL-ARRAY (SORT-VECTOR-SMALLEST-INDEX COMB VEC)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(3 1 4))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE-FLOAT
		 (A 7.1 -2.0 12.8 -3.21 1.0)))
	   (SM (COMBINATION '(5 3))))
      (CL-ARRAY (SORT-SMALLEST-INDEX SM VEC)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(12.8 7.1 1.0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE-FLOAT (A 7.1 -2.0 12.8 -3.21 1.0)))
	   (LARGEST (VECTOR-DOUBLE-FLOAT 3)))
      (SORT-VECTOR-LARGEST LARGEST VEC)
      (CL-ARRAY LARGEST))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(2 0 4))
   (MULTIPLE-VALUE-LIST
    (LETM
	((VEC (VECTOR-DOUBLE-FLOAT (A 7.1 -2.0 12.8 -3.21 1.0)))
	 (COMB (COMBINATION '(5 3))))
      (CL-ARRAY (SORT-VECTOR-LARGEST-INDEX COMB VEC))))))
