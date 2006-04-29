;********************************************************
; file:        sorting.lisp                              
; description: Sorting                                   
; date:        Fri Apr 14 2006 - 20:20                   
; author:      Liam M. Healy                             
; modified:    Fri Apr 28 2006 - 22:21
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; #'heapsort has just a cursory port, use CL's #'sort.
;;; Raw C array functions not ported, there is no point.
;;; Need vectors over integers for
;;; gsl_sort_smallest_index, gsl_sort_largest_index
;;; need to return converted raw C arrays.

;;;;****************************************************************************
;;;; Heapsort, not recommended
;;;;****************************************************************************

(defmacro defcomparison (name &body body)
  `(cffi:defcallback ,name :int ((a :pointer) (b :pointer))
    ,body))

(defun-gsl heapsort
    ((array :pointer) (count :size) (size :size) (function :pointer))
  "gsl_heapsort"
  :documentation
  "Sort the @var{count} elements of the array @var{array},
   each of size @var{size}, into ascending order using the comparison
   function @var{compare}.  The type of the comparison function is defined by,
   A comparison function should return a negative integer if the first
   argument is less than the second argument, @code{0} if the two arguments
   are equal and a positive integer if the first argument is greater than
   the second argument."
  :c-return-value :void)

(defun-gsl heapsort-index
    ((p :size) (array :pointer) (count :size) (size :size) (function :pointer))
  "gsl_heapsort_index"
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

(defun-gsl sort-vector ((vector gsl-vector-c))
  "gsl_sort_vector"
  :documentation
  "Sort the elements of the vector into ascending numerical order."
  :c-return-value :void)

(defun-gsl sort-vector-index
    ((permutation gsl-permutation-c) (vector gsl-vector-c))
  "gsl_sort_vector_index"
  :documentation
  "Indirectly sort the elements of the vector @var{v} into
ascending order, storing the resulting permutation in @var{p}.  The
elements of @var{p} give the index of the vector element which would
have been stored in that position if the vector had been sorted in
place.  The first element of @var{p} gives the index of the least element
in @var{v}, and the last element of @var{p} gives the index of the
greatest element in @var{v}.  The vector @var{v} is not changed.")

(defun-gsl sort-vector-smallest
    (((gsl-array dest) :pointer) ((dim0 dest) :size) (v gsl-vector-c))
  "gsl_sort_vector_smallest"
  :function (dest v)
  :documentation
  "Find the smallest elements of the vector @var{v} and put them into dest,
   which must be shorter than v."
  :c-return-value :void
  :after ((cl-invalidate dest))
  :return-input (dest))

(defun-gsl sort-vector-largest
    (((gsl-array dest) :pointer) ((dim0 dest) :size) (v gsl-vector-c))
  "gsl_sort_vector_largest"
  :function (dest v)
  :documentation
  "Find the largest elements of the vector @var{v} and put them into dest,
   which must be shorter than v."
  :c-return-value :void
  :after ((cl-invalidate dest))
  :return-input (dest))

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

#|
(with-data (vec vector 5)
  (setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
  (sort-vector vec)
  (data vec))
;;; #(-3.21d0 -2.0d0 1.0d0 7.1d0 12.8d0)

(with-data (perm permutation 5)
  (with-data (vec vector 5)
    (setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
    (sort-vector-index perm vec)
    (data perm)))
#(3 1 4 0 2)

(defparameter vec (make-data 'vector nil 5))
(setf (data vec) #(7.1d0 -2.0d0 12.8d0 -3.21d0 1.0d0))
(defparameter smallest (make-data 'vector nil 3))
(sort-vector-smallest smallest vec)
(free vec)
(free smallest)

|#
