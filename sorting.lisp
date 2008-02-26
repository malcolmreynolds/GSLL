;; Sorting
;; Liam Healy, Fri Apr 14 2006 - 20:20
;; Time-stamp: <2008-02-17 10:08:41EST sorting.lisp>
;; $Id$

(in-package :gsl)

;;; #'heapsort has just a cursory port, use CL's #'sort.
;;; Raw C array functions not ported, not policy.

;;; Errors:
;;; sort-vector-smallest-index and sort-vector-largest-index
;;; do not work on amd64/SBCL, and should be defined in terms of
;;; vector-unsigned-fixnum anyway

;;;;****************************************************************************
;;;; Heapsort, not recommended
;;;;****************************************************************************

(defmacro defcomparison (name &body body)
  `(cffi:defcallback ,name :int ((a :pointer) (b :pointer))
    ,body))

(defmfun heapsort (array count size function)
  "gsl_heapsort"
  ((array :pointer) (count size) (size size) (function :pointer))
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
  ((p size) (array :pointer) (count size) (size size) (function :pointer))
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
;;;; Vector sort
;;;;****************************************************************************

;;; Port only _vector_ sort functions?

(export 'sort-vector)
(defgeneric sort-vector (vector)
  (:documentation			; FDL
   "Sort the elements of the vector into ascending numerical order."))

(defmfun-vdsf sort-vector ((vector gsl-vector))
  "gsl_sort_vector" (((pointer vector) gsl-vector-c))
  :c-return :void)

(export 'sort-vector-index)
(defgeneric sort-vector-index (permutation vector)
  (:documentation			; FDL
   "Indirectly sort the elements of the vector v into
   ascending order, storing the resulting permutation in p.  The
   elements of p give the index of the vector element which would
   have been stored in that position if the vector had been sorted in
   place.  The first element of p gives the index of the least element
   in v and the last element of p gives the index of the
   greatest element in v.  The vector v is not changed."))

(defmfun-vdsf sort-vector-index (permutation (vector gsl-vector))
  "gsl_sort_vector_index"
  (((pointer permutation) gsl-permutation-c)
   ((pointer vector) gsl-vector-c)))

(export 'sort-vector-smallest)
(defgeneric sort-vector-smallest (destination vector)
  (:documentation			; FDL
   "Find the smallest elements of the vector v and put them into dest,
   which must be shorter than v."))

(defmfun-vdsf sort-vector-smallest (dest (v gsl-vector))
  "gsl_sort_vector_smallest"
  (((gsl-array dest) :pointer) ((dim0 dest) size)
   ((pointer v) gsl-vector-c))
  :c-return :void
  :invalidate (dest))

;;; p should be gsl-vector-unsigned-fixnum, if that can be made to
;;; work (see vector.lisp).
(export 'sort-vector-smallest-index)
(defgeneric sort-vector-smallest-index (p vector)
  (:documentation			; FDL
   "The indices of the smallest elements of the vector stored
   in the array p."))

(defmfun-vdsf sort-vector-smallest-index
    ((p gsl-vector-fixnum) (v gsl-vector))
  "gsl_sort_vector_smallest_index"
  (((gsl-array p) :pointer) ((dim0 p) size)
   ((pointer v) gsl-vector-c))
  :c-return :void
  :invalidate (p))

(export 'sort-vector-largest)
(defgeneric sort-vector-largest (dest vector)
  (:documentation			; FDL
  "Find the largest elements of the vector and put them into dest,
   which must be shorter than the vector."))

(defmfun-vdsf sort-vector-largest (dest (v gsl-vector))
  "gsl_sort_vector_largest"
  (((gsl-array dest) :pointer) ((dim0 dest) size)
   ((pointer v) gsl-vector-c))
  :c-return :void
  :invalidate (dest))

(export 'sort-vector-largest-index)
;;; p should be gsl-vector-unsigned-fixnum, if that can be made to
;;; work (see vector.lisp).
(defgeneric sort-vector-largest-index (p vector)
  (:documentation			; FDL
   "The indices of the largest elements of the vector stored
   in the array p."))

(defmfun-vdsf sort-vector-largest-index
    ((p gsl-vector-fixnum) (v gsl-vector))
  "gsl_sort_vector_largest_index"
  (((gsl-array p) :pointer) ((dim0 p) size)
   ((pointer v) gsl-vector-c))
  :c-return :void
  :invalidate (p))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests sorting
 (letm ((vec (vector-double #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))))
   (sort-vector vec)
   (data vec))
 (letm ((perm (permutation 5))
	(vec (vector-double #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))))
   (sort-vector-index perm vec)
   (data perm))
 (letm ((vec (vector-double #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(smallest (vector-double 3)))
   (sort-vector-smallest smallest vec)
   (data smallest))
 (letm ((vec (vector-double #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(smallest (vector-fixnum 3)))
   (sort-vector-smallest-index smallest vec)
   (data smallest))
 (letm ((vec (vector-double #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(largest (vector-double 3)))
   (sort-vector-largest largest vec)
   (data largest))
 (letm ((vec (vector-double #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	(largest (vector-fixnum 3)))
   (sort-vector-largest-index largest vec)
   (data largest)))
|#

(LISP-UNIT:DEFINE-TEST SORTING
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(-3.21d0 -2.0d0 1.0d0 7.1d0 12.8d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))))
      (SORT-VECTOR VEC) (DATA VEC))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(3 1 4 0 2))
   (MULTIPLE-VALUE-LIST
    (LETM ((PERM (PERMUTATION 5))
	 (VEC (VECTOR-DOUBLE #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))))
      (SORT-VECTOR-INDEX PERM VEC)
      (DATA PERM))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(-3.21d0 -2.0d0 1.0d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	 (SMALLEST (VECTOR-DOUBLE 3)))
      (SORT-VECTOR-SMALLEST SMALLEST VEC)
      (DATA SMALLEST))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(3 1 4))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	 (SMALLEST (VECTOR-FIXNUM 3)))
      (SORT-VECTOR-SMALLEST-INDEX
       SMALLEST VEC)
      (DATA SMALLEST))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(12.8d0 7.1d0 1.0d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	 (LARGEST (VECTOR-DOUBLE 3)))
      (SORT-VECTOR-LARGEST LARGEST VEC) (DATA LARGEST))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(2 0 4))
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0)))
	 (LARGEST (VECTOR-FIXNUM 3)))
      (SORT-VECTOR-LARGEST-INDEX LARGEST VEC)
      (DATA LARGEST)))))
