;; Structures returned by special functions.
;; Liam Healy, Mon Jan  1 2007 - 11:35
;; Time-stamp: <2009-06-06 10:32:44EDT return-structures.lisp>

(in-package :gsl)

;;;;****************************************************************************
;;;; Result from special functions
;;;;****************************************************************************

(defun val (sf-result &optional (type 'sf-result))
  (cffi:foreign-slot-value sf-result type 'val))

(defun err (sf-result &optional (type 'sf-result))
  (cffi:foreign-slot-value sf-result type 'err))

(defun e10 (sf-result)
  (cffi:foreign-slot-value sf-result 'sf-result-e10 'e10))

;;;;****************************************************************************
;;;; Array construction
;;;;****************************************************************************

(defparameter *default-sf-array-size* 5
  "The default size to make an array returned from a special function.")

(defun vdf (size-or-array)
  "Make or take a vector-double-float."
  (if (integerp size-or-array)
      (make-marray 'double-float :dimensions size-or-array)
      size-or-array))

(defun vdf-size (size-or-array)
  "Make or take a vector-double-float."
  (if (integerp size-or-array)
      size-or-array
      (size size-or-array)))
