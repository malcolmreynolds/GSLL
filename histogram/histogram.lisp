;********************************************************
; file:        structure.lisp                            
; description: The histogram structure.                  
; date:        Mon Jan  1 2007 - 11:32                   
; author:      Liam M. Healy                             
; modified:    Tue Jan  2 2007 - 23:03
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Define, make, copy histograms in one or two dimensions.

;;; Should this have a with-histogram macro, or be integrated into
;;; the data superclass (and so use with-data)?

(defclass histogram ()
  ((pointer
    :initarg :pointer :accessor pointer
    :documentation "A C pointer to the GSL representation of the histogram.")
   (number-of-bins :initarg :number-of-bins :accessor number-of-bins))
  (:documentation
   "A histogram, including bin boundaries and bin contents."))

(defun-gsl alloc-histo-1 (object)
  "gsl_histogram_alloc"
  (((number-of-bins object) :size))
  :export nil
  :index alloc
  :c-return (cr :pointer)
  :return ((assign-pointer object cr)))

(defun-gsl alloc-histo-2 (object)
  "gsl_histogram2d_alloc"
  (((first (number-of-bins object)) :size)
   ((second (number-of-bins object)) :size))
  :export nil
  :index alloc
  :c-return (cr :pointer)
  :return ((assign-pointer object cr)))

(defmacro histo-1d2d
    (object base-function-name &optional args1 args2)
  `(if (listp (number-of-bins ,object))
       (,(intern
	  (concatenate 'string (string base-function-name) "-2"))
	 ,object ,@(or args2 args1))
       (,(intern
	  (concatenate 'string (string base-function-name) "-1"))
	 ,object ,@args1)))

(defmethod alloc ((object histogram))
  (histo-1d2d object alloc-histo))

(defun-gsl free-histo-1 (object)
  "gsl_histogram_free"
  (((pointer object) :pointer))
  :c-return (cr :pointer)
  :return ((assign-pointer object cr)))

(defun-gsl free-histo-2 (object)
  "gsl_histogram2d_free"
  (((pointer object) :pointer))
  :c-return (cr :pointer)
  :return ((assign-pointer object cr)))

(defmethod free ((object histogram))
  (histo-1d2d object free-histo))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl set-ranges-1 (histogram ranges)
  "gsl_histogram_set_ranges"
  (((pointer histogram) :pointer)
   ((gsl-array ranges) :pointer) ((dim0 ranges) :size))
  :export nil
  :index set-ranges
  :documentation
  "Set the ranges of the existing histogram using
   the gsl-vector of ranges.  The values of the histogram
   bins are reset to zero.  The @code{ranges} array should contain the
   desired bin limits.  The ranges can be arbitrary, subject to the
   restriction that they are monotonically increasing.")

(defun-gsl set-ranges-2 (histogram x-ranges y-ranges)
  "gsl_histogram2d_set_ranges"
  (((pointer histogram) :pointer)
   ((gsl-array x-ranges) :pointer) ((dim0 x-ranges) :size)
   ((gsl-array y-ranges) :pointer) ((dim0 y-ranges) :size))
  :export nil
  :index set-ranges)

(export 'set-ranges)
(defun set-ranges (histogram &rest ranges)
  "Set the ranges of the existing histogram using
   the gsl-vector(s) of ranges.  The values of the histogram
   bins are reset to zero.  The ranges array(s) should contain the
   desired bin limits.  The ranges can be arbitrary, subject to the
   restriction that they are monotonically increasing.
   For a 2d histogram, supply two gsl-vectors."
  (histo-1d2d histogram set-ranges
	      ((first ranges))
	      ((first ranges) (second ranges))))

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl set-ranges-uniform-1 (histogram minimum maximum)
  "gsl_histogram_set_ranges_uniform"
  (((pointer histogram) :pointer) (minimum :double) (maximum :double))
  :export nil
  :index set-ranges-uniform
  :documentation
  "Set the ranges of the existing histogram @var{h} to cover
   the range @var{xmin} to @var{xmax} uniformly.  The values of the
   histogram bins are reset to zero.  The bin ranges are shown in the table
   below,
   bin[0] corresponds to xmin <= x < xmin + d
   bin[1] corresponds to xmin + d <= x < xmin + 2 d
   ......
   bin[n-1] corresponds to xmin + (n-1)d <= x < xmax
   where @math{d} is the bin spacing, @math{d = (xmax-xmin)/n}.")

;;; GSL documentation does not state what the return value for the
;;; C function means; assumed to be error code.
(defun-gsl set-ranges-uniform-2
    (histogram x-minimum x-maximum y-minimum y-maximum)
  "gsl_histogram2d_set_ranges_uniform"
  (((pointer histogram) :pointer)
   (x-minimum :double) (x-maximum :double)
   (y-minimum :double) (y-maximum :double))
  :export nil
  :index set-ranges-uniform)

(export 'set-ranges-uniform)
(defun set-ranges-uniform (histogram &rest limits)
  "Set the ranges of the existing histogram @var{h} to cover
   the range @var{xmin} to @var{xmax} uniformly.  The values of the
   histogram bins are reset to zero.  The bin ranges are shown in the table
   below,
   bin[0] corresponds to xmin <= x < xmin + d
   bin[1] corresponds to xmin + d <= x < xmin + 2 d
   ......
   bin[n-1] corresponds to xmin + (n-1)d <= x < xmax
   where @math{d} is the bin spacing, @math{d = (xmax-xmin)/n}."
  (histo-1d2d histogram set-ranges-uniform
	      ((first limits) (second limits))
	      ((first limits) (second limits)
	       (third limits) (fourth limits))))

(defun-gsl copy-1 (destination source)
  "gsl_histogram_memcpy"
  (((pointer destination) :pointer) ((pointer source) :pointer))
  :export nil
  :index copy
  :documentation
  "Copy the histogram source into the pre-existing
   histogram destination, making the latter into
   an exact copy of the former.
   The two histograms must be of the same size.")

(defun-gsl copy-2 (destination source)
  "gsl_histogram2d_memcpy"
  (((pointer destination) :pointer) ((pointer source) :pointer))
  :export nil
  :index copy)

(defmethod copy ((destination histogram) (source histogram))
  (histo-1d2d destination copy (source)))

(defun-gsl clone-1 (source)
  "gsl_histogram_memcpy"
  (((pointer source) :pointer))
  :export nil
  :index clone
  :documentation
  "Create a new histogram which is an
   exact copy of the histogram source, and return the pointer.")

(defun-gsl clone-2 (source)
  "gsl_histogram2d_memcpy"
  (((pointer source) :pointer))
  :export nil
  :index clone
  :documentation
  "Create a new histogram which is an
   exact copy of the histogram source, and return the pointer.")

(export 'clone)
(defun clone (source)
  (histo-1d2d source clone))

(defun make-histogram (size &optional from)
  "Make a histogram, optionally filling it with
   data from an existing histogram."
  (let ((ret
	 (make-instance 'histogram :number-of-bins size
			:pointer (when from (clone from)))))
    (unless (pointer ret)
      (alloc ret))
    ret))


