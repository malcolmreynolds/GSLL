;********************************************************
; file:        sorting.lisp                              
; description: Sorting                                   
; date:        Fri Apr 14 2006 - 20:20                   
; author:      Liam M. Healy                             
; modified:    Wed Jun 21 2006 - 23:29
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; #'heapsort has just a cursory port, use CL's #'sort.
;;; Raw C array functions not ported, not policy.

;;;;****************************************************************************
;;;; Heapsort, not recommended
;;;;****************************************************************************

(defmacro defcomparison (name &body body)
  `(cffi:defcallback ,name :int ((a :pointer) (b :pointer))
    ,body))

(defun-gsl heapsort (array count size function)
  "gsl_heapsort"
  ((array :pointer) (count :size) (size :size) (function :pointer))
  :documentation
  "Sort the @var{count} elements of the array @var{array},
   each of size @var{size}, into ascending order using the comparison
   function @var{compare}.  The type of the comparison function is defined by,
   A comparison function should return a negative integer if the first
   argument is less than the second argument, @code{0} if the two arguments
   are equal and a positive integer if the first argument is greater than
   the second argument."
  :c-return :void)

(defun-gsl heapsort-index (p array count size function)
  "gsl_heapsort_index"
  ((p :size) (array :pointer) (count :size) (size :size) (function :pointer))
  :documentation
  "Indirectly sort the @var{count} elements of the array
   @var{array}, each of size @var{size}, into ascending order using the
   comparison function @var{compare}.  The resulting permutation is stored
   in @var{p}, an array of length @var{n}.  The elements of @var{p} give the
   index of the array element which would have been stored in that position
   if the array had been sorted in place.  The first element of @var{p}
   gives the index of the least element in @var{array}, and the last
   element of @var{p} gives the index of the greatest element in
   @var{array}.  The array itself is not changed.")

;;;;****************************************************************************
;;;; Vector sort
;;;;****************************************************************************

;;; Port only _vector_ sort functions?

(defun-gsl-vdsf sort-vector ((vector gsl-vector))
  "gsl_sort_vector" (((pointer vector) gsl-vector-c))
  :documentation
  "Sort the elements of the vector into ascending numerical order."
  :c-return :void)

(defun-gsl-vdsf sort-vector-index (permutation (vector gsl-vector))
  "gsl_sort_vector_index"
  (((pointer permutation) gsl-permutation-c)
   ((pointer vector) gsl-vector-c))
  :documentation
  "Indirectly sort the elements of the vector @var{v} into
   ascending order, storing the resulting permutation in @var{p}.  The
   elements of @var{p} give the index of the vector element which would
   have been stored in that position if the vector had been sorted in
   place.  The first element of @var{p} gives the index of the least element
   in @var{v}, and the last element of @var{p} gives the index of the
   greatest element in @var{v}.  The vector @var{v} is not changed.")

(defun-gsl-vdsf sort-vector-smallest (dest (v gsl-vector))
  "gsl_sort_vector_smallest"
  (((gsl-array dest) :pointer) ((dim0 dest) :size)
   ((pointer v) gsl-vector-c))
  :documentation
  "Find the smallest elements of the vector @var{v} and put them into dest,
   which must be shorter than v."
  :c-return :void
  :invalidate (dest))

;;; This is not right in that p should be over uint, not int.
(defun-gsl-vdsf sort-vector-smallest-index
    ((p gsl-vector-fixnum) (v gsl-vector))
  "gsl_sort_vector_smallest_index"
  (((gsl-array p) :pointer) ((dim0 p) :size)
   ((pointer v) gsl-vector-c))
  :documentation
  "The indices of the smallest elements of the vector @var{v} stored
   in the array @var{p}."
  :c-return :void
  :invalidate (p))

(defun-gsl-vdsf sort-vector-largest (dest (v gsl-vector))
  "gsl_sort_vector_largest"
  (((gsl-array dest) :pointer) ((dim0 dest) :size)
   ((pointer v) gsl-vector-c))
  :documentation
  "Find the largest elements of the vector @var{v} and put them into dest,
   which must be shorter than v."
  :c-return :void
  :invalidate (dest))

;;; This is not right in that p should be over uint, not int.
(defun-gsl-vdsf sort-vector-largest-index
    ((p gsl-vector-fixnum) (v gsl-vector))
  "gsl_sort_vector_largest_index"
  (((gsl-array p) :pointer) ((dim0 p) :size)
   ((pointer v) gsl-vector-c))
  :documentation
  "The indices of the largest elements of the vector @var{v} stored
   in the array @var{p}."
  :c-return :void
  :invalidate (p))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test sorting
  (lisp-unit:assert-equal
   '("-0.321000000000d+01" "-0.200000000000d+01" "0.100000000000d+01"
     "0.710000000000d+01" "0.128000000000d+02")
   (lisp-unit:fp-sequence
    (with-data (vec vector-double 5)
      (setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
      (sort-vector vec)
      (data vec))))
  (lisp-unit:assert-equalp
   #(3 1 4 0 2)
   (with-data (perm permutation 5)
     (with-data (vec vector-double 5)
       (setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
       (sort-vector-index perm vec)
       (data perm))))
  (lisp-unit:assert-equal
   '("-0.321000000000d+01" "-0.200000000000d+01" "0.100000000000d+01")
   (lisp-unit:fp-sequence
    (with-data (vec vector-double 5)
      (with-data (smallest vector-double 3)
	(setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
	(sort-vector-smallest smallest vec)
	(data smallest)))))
  (lisp-unit:assert-equalp
   #(3 1 4)
   (with-data (vec vector-double 5)
     (with-data (smallest vector-fixnum 3)
       (setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
       (sort-vector-smallest-index smallest vec)
       (data smallest))))
  (lisp-unit:assert-equal
   '("0.128000000000d+02" "0.710000000000d+01" "0.100000000000d+01")
   (lisp-unit:fp-sequence
    (with-data (vec vector-double 5)
      (with-data (largest vector-double 3)
	(setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
	(sort-vector-largest largest vec)
	(data largest)))))
  (lisp-unit:assert-equalp
   #(2 0 4)
   (with-data (vec vector-double 5)
     (with-data (largest vector-fixnum 3)
       (setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
       (sort-vector-largest-index largest vec)
       (data largest)))))

