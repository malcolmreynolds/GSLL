;; Functions for both vectors and matrices.
;; Liam Healy 2008-04-26 20:48:44EDT both.lisp
;; Time-stamp: <2008-06-11 21:42:11EDT both.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Administrative (internal use)
;;;;****************************************************************************

(defmfun alloc-from-block ((object vector))
  ("gsl_" :category :type "_alloc_from_block")
  (((block-pointer object) :pointer)	
   (0 size)				; offset
   (totsize size)			; number of elements
   (1 size))				; stride
  :definition :generic
  :global ((totsize (total-size object)))
  :c-return :pointer
  :export nil
  :documentation "Allocate memory for the GSL struct given a block pointer.")

(defmfun alloc-from-block ((object matrix))
  ("gsl_" :category :type "_alloc_from_block")
  (((block-pointer object) :pointer)
   (0 size)				; offset
   ((first (dimensions object)) size)	; number of rows
   ((second (dimensions object)) size)	; number of columns
   ((second (dimensions object)) size))	; "tda" = number of columns for now
  :definition :method
  :c-return :pointer
  :export nil)

;;;;****************************************************************************
;;;; Bulk operations
;;;;****************************************************************************

(defmfun set-all ((object both) value)
  ("gsl_" :category :type "_set_all")
  (((mpointer object) :pointer) (value :element-c-type))
  :definition :generic
  :inputs (object)
  :outputs (object)
  :c-return :void
  :documentation "Set all elements to the value.")

(defmfun set-zero ((object both))
  ("gsl_"  :category :type "_set_zero")
  (((mpointer object) :pointer))
  :definition :generic
  :inputs (object)
  :outputs (object)
  :c-return :void
  :documentation "Set all elements to 0.")

(defmfun copy ((destination both) (source both))
  ("gsl_" :category :type "_memcpy")
  (((mpointer destination) :pointer) ((mpointer source) :pointer))
  :definition :generic
  :inputs (source)
  :outputs (destination)
  :documentation			; FDL
  "Copy the elements of the source into the
   destination.  The two must have the same size.")

(defmfun swap ((a both) (b both))
  ("gsl_" :category :type "_swap")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :inputs (a b)
  :outputs (a b)
  :documentation			; FDL
  "Exchange the elements of a and b
   by copying.  The two must have the same dimensions.")

;;; These functions are here as debugging tools only; they do not insure
;;; coherency between the C and CL sides and therefore should not be
;;; used in production.
(defmfun set-value ((object both) index value)
  ("gsl_"  :category :type "_set")
  (((mpointer object) :pointer) (index size) (value :element-c-type))
  :definition :generic
  :inputs (object)
  :outputs (object)
  :c-return :void
  :export nil				; debugging only
  :documentation
  "Set single element to the value.  For debugging only; do not use.")

(defmfun get-value ((object both) index)
  ("gsl_"  :category :type "_get")
  (((mpointer object) :pointer) (index size))
  :definition :generic
  :inputs (object)
  :c-return :element-c-type
  :export nil				; debugging only
  :documentation
  "Set single element to the value.  For debugging only; do not use.")

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(defmfun m+ ((a both) (b both))
  ("gsl_" :category :type "_add")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types :no-complex		; Question for GSL: why no complex?
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Add the elements of b to the elements of vector a
   The two must have the same dimensions.")

(defmfun m- ((a both) (b both))
  ("gsl_" :category :type "_sub")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types :no-complex		; Question for GSL: why no complex?
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Subtract the elements of b from the elements of a.
   The two must have the same dimensions.")

(defmfun m* ((a vector) (b vector))
  ("gsl_" :category :type "_mul")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types :no-complex		; Question for GSL: why no complex?
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Multiply the elements of a by the elements of b.
   The two must have the same dimensions.")

(defmfun m* ((a matrix) (b matrix))
  ("gsl_" :category :type "_mul_elements")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :method
  :element-types :no-complex		; Question for GSL: why no complex?
  :inputs (a b)
  :outputs (a)
  :return (a))

(defmfun m/ ((a vector) (b vector))
  ("gsl_" :category :type "_div")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types :no-complex		; Question for GSL: why no complex?
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Divide the elements of a by the elements of b.
   The two must have the same dimensions.")

(defmfun m/ ((a matrix) (b matrix))
  ("gsl_" :category :type "_div_elements")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types :no-complex		; Question for GSL: why no complex?
  :inputs (a b)
  :outputs (a)
  :return (a))

(defmfun m*c ((a both) x)
  ("gsl_" :category :type "_scale")
  (((mpointer a) :pointer) (x :element-c-type))
  :definition :generic
  :element-types :no-complex		; Question for GSL: why no complex?
  :inputs (a)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Multiply the elements of a by the constant factor x.")

(defmfun m+c ((a both) x)
  ("gsl_" :category :type "_add_constant")
  (((mpointer a) :pointer) (x :element-c-type))
  :definition :generic
  :element-types :no-complex		; Question for GSL: why no complex?
  :inputs (a)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Add the constant value x to the elements of the a.")

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(defmfun mmax ((a both))
  ("gsl_" :category :type "_max")
  (((mpointer a) :pointer))
  :definition :generic
  :element-types :no-complex
  :inputs (a)
  :c-return :element-c-type
  :documentation			; FDL
  "The maximum value in a.")

(defmfun mmin ((a both))
  ("gsl_" :category :type "_min")
  (((mpointer a) :pointer))
  :definition :generic
  :element-types :no-complex
  :inputs (a)
  :c-return :element-c-type
  :documentation			; FDL
  "The minimum value in a.")

(defmfun minmax ((a both))
  ("gsl_" :category :type "_minmax")
  (((mpointer a) :pointer) (min :element-c-type) (max :element-c-type))
  :definition :generic
  :element-types :no-complex
  :inputs (a)
  :c-return :void
  :documentation			; FDL
  "The minimum and maximum values in a.")

(defmfun min-index ((a vector))
  ("gsl_" :category :type "_min_index")
  (((mpointer a) :pointer))
  :definition :generic
  :element-types :no-complex
  :inputs (a)
  :c-return size
  :documentation			; FDL
  "The index of the minimum value in a.  When there are several
  equal minimum elements, then the lowest index is returned.")

(defmfun min-index ((a matrix))
  ("gsl_" :category :type "_min_index")
  (((mpointer a) :pointer) (imin size) (jmin size))
  :definition :method
  :element-types :no-complex
  :inputs (a)
  :c-return :void)

(defmfun max-index ((a vector))
  ("gsl_" :category :type "_max_index")
  (((mpointer a) :pointer))
  :definition :generic
  :element-types :no-complex
  :inputs (a)
  :c-return size
  :documentation			; FDL
  "The index of the maximum value in a.  When there are several
  equal maximum elements, then the lowest index is returned.")

(defmfun max-index ((a matrix))
  ("gsl_" :category :type "_max_index")
  (((mpointer a) :pointer) (imax size) (jmax size))
  :definition :method
  :element-types :no-complex
  :inputs (a)
  :c-return :void)

(defmfun minmax-index ((a vector))
  ("gsl_" :category :type "_minmax_index")
  (((mpointer a) :pointer) (imin size) (imax size))
  :definition :generic
  :element-types :no-complex
  :inputs (a)
  :c-return :void
  :documentation			; FDL
  "The indices of the minimum and maximum values in a.
  When there are several equal minimum elements then the lowest index is
  returned.  Returned indices are minimum, maximum; for matrices
  imin, jmin, imax, jmax.")

(defmfun minmax-index ((a matrix))
  ("gsl_" :category :type "_minmax_index")
  (((mpointer a) :pointer) (imin size) (jmin size) (imax size) (jmax size))
  :definition :method
  :element-types :no-complex
  :inputs (a)
  :c-return :void)

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

(defmfun mzerop ((a both))
  ("gsl_" :category :type "_isnull")
  (((mpointer a) :pointer))
  :definition :generic
  :inputs (a)
  :c-return :boolean
  :documentation			; FDL
  "All elements of a are zero.")
