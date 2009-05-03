;; Functions for both vectors and matrices.
;; Liam Healy 2008-04-26 20:48:44EDT both.lisp
;; Time-stamp: <2009-05-03 11:24:50EDT both.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Administrative (internal use)
;;;;****************************************************************************

(defmfun alloc-from-block ((object vector) blockptr)
  ("gsl_" :category :type "_alloc_from_block")
  ((blockptr :pointer)
   (0 sizet)				; offset
   ((total-size object) sizet)			; number of elements
   (1 sizet))				; stride
  :definition :generic
  :c-return :pointer
  :export nil
  :documentation "Allocate memory for the GSL struct given a block pointer.")

(defmfun alloc-from-block ((object matrix) blockptr)
  ("gsl_" :category :type "_alloc_from_block")
  ((blockptr :pointer)
   (0 sizet)				; offset
   ((first (dimensions object)) sizet)	; number of rows
   ((second (dimensions object)) sizet)	; number of columns
   ((second (dimensions object)) sizet))	; "tda" = number of columns for now
  :definition :methods
  :c-return :pointer
  :export nil)

;;;;****************************************************************************
;;;; Bulk operations
;;;;****************************************************************************

(defmfun set-all ((object both) value)
  ("gsl_" :category :type "_set_all")
  (((mpointer object) :pointer) (value :element-c-type))
  :definition :generic
  :element-types #+fsbv t #-fsbv :no-complex
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

(defmfun copy-to-destination
    ((source both) (destination both))
  ("gsl_" :category :type "_memcpy")
  (((mpointer destination) :pointer) ((mpointer source) :pointer))
  :definition :methods
  :inputs (source)
  :outputs (destination))

(defmfun copy-making-destination
    ((source both) &aux 
     (destination
      (make-marray element-type :dimensions (dimensions source))))
  ("gsl_" :category :type "_memcpy")
  (((mpointer destination) :pointer) ((mpointer source) :pointer))
  :definition :methods
  :inputs (source)
  :outputs (destination)
  :return (destination))

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
;;; For matrix functions, there should be another index argument
(defmfun set-value ((object vector) index value)
  ("gsl_"  :category :type "_set")
  (((mpointer object) :pointer) (index sizet) (value :element-c-type))
  :definition :generic
  :element-types #+fsbv t #-fsbv :no-complex
  :inputs (object)
  :outputs (object)
  :c-return :void
  :export nil				; debugging only
  :documentation
  "Set single element to the value.  For debugging only; do not use.")

(defmfun get-value ((object vector) index)
  ("gsl_"  :category :type "_get")
  (((mpointer object) :pointer) (index sizet))
  :definition :generic
  :inputs (object)
  :c-return :element-c-type
  :export nil				; debugging only
  :documentation
  "Set single element to the value.  For debugging only; do not use.")

;;;;****************************************************************************
;;;; Elementwise arithmetic operations overwriting an array
;;;;****************************************************************************

;;; These are overwriting elementwise arithmetic operations.  Where
;;; there are two array arguments, the operation acts on both arrays
;;; on corresponding elements.  The result is placed in the first
;;; array, overwriting the original contents.

;;; Errors in GSL:
;;; 1) complex operations in older versions of GSL
;;; https://savannah.gnu.org/bugs/index.php?22478
;;; Fixed in 1.12, can change the :no-complex spec.

(defmfun elt+ ((a both) (b both))
  ("gsl_" :category :type "_add")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types #.(if (have-at-least-gsl-version '(1 12)) t :no-complex)
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Add the elements of b to the elements of vector a
   The two must have the same dimensions.")

(defmfun elt+ ((a both) (x float))
  ("gsl_" :category :type "_add_constant")
  (((mpointer a) :pointer) (x :double))
  :definition :methods
  :element-types #.(if (have-at-least-gsl-version '(1 12)) t :no-complex)
  :inputs (a)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Add the scalar double-float x to all the elements of array a.")

(defmethod elt+ ((x float) (a marray))
  (elt+ a x))
  
(defmfun elt- ((a both) (b both))
  ("gsl_" :category :type "_sub")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types #.(if (have-at-least-gsl-version '(1 12)) t :no-complex)
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Subtract the elements of b from the elements of a.
   The two must have the same dimensions.")

(defmethod elt- ((a marray) (x float))
  (elt+ a (- x)))

(defmfun elt* ((a vector) (b vector))
  ("gsl_" :category :type "_mul")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types #.(if (have-at-least-gsl-version '(1 12)) t :no-complex)
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Multiply the elements of a by the elements of b.
   The two must have the same dimensions.")

(defmfun elt* ((a matrix) (b matrix))
  ("gsl_" :category :type "_mul_elements")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :methods
  :element-types :no-complex
  :inputs (a b)
  :outputs (a)
  :return (a))

(defmfun elt/ ((a vector) (b vector))
  ("gsl_" :category :type "_div")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :generic
  :element-types #.(if (have-at-least-gsl-version '(1 12)) t :no-complex)
  :inputs (a b)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Divide the elements of a by the elements of b.
   The two must have the same dimensions.")

(defmfun elt/ ((a matrix) (b matrix))
  ("gsl_" :category :type "_div_elements")
  (((mpointer a) :pointer) ((mpointer b) :pointer))
  :definition :methods
  :element-types :no-complex
  :inputs (a b)
  :outputs (a)
  :return (a))

(defmethod elt/ ((a marray) (x float))
  (elt* a (/ x)))

(defmfun elt* ((a both) (x float))
  ("gsl_" :category :type "_scale")
  (((mpointer a) :pointer) (x :double))
  :definition :methods
  :element-types #.(if (have-at-least-gsl-version '(1 12)) t :no-complex)
  :inputs (a)
  :outputs (a)
  :return (a)
  :documentation			; FDL
  "Multiply the elements of a by the scalar double-float factor x.")

(defmethod elt* ((x float) (a marray))
  (elt* a x))

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
  (((mpointer a) :pointer)
   (min (:pointer :element-c-type))
   (max (:pointer :element-c-type)))
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
  :c-return sizet
  :documentation			; FDL
  "The index of the minimum value in a.  When there are several
  equal minimum elements, then the lowest index is returned.")

(defmfun min-index ((a matrix))
  ("gsl_" :category :type "_min_index")
  (((mpointer a) :pointer) (imin (:pointer sizet)) (jmin (:pointer sizet)))
  :definition :methods
  :element-types :no-complex
  :inputs (a)
  :c-return :void)

(defmfun max-index ((a vector))
  ("gsl_" :category :type "_max_index")
  (((mpointer a) :pointer))
  :definition :generic
  :element-types :no-complex
  :inputs (a)
  :c-return sizet
  :documentation			; FDL
  "The index of the maximum value in a.  When there are several
  equal maximum elements, then the lowest index is returned.")

(defmfun max-index ((a matrix))
  ("gsl_" :category :type "_max_index")
  (((mpointer a) :pointer) (imin (:pointer sizet)) (jmin (:pointer sizet)))
  :definition :methods
  :element-types :no-complex
  :inputs (a)
  :c-return :void)

(defmfun minmax-index ((a vector))
  ("gsl_" :category :type "_minmax_index")
  (((mpointer a) :pointer) (imin (:pointer sizet)) (jmin (:pointer sizet)))
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
  (((mpointer a) :pointer)
   (imin (:pointer sizet)) (jmin (:pointer sizet))
   (imax (:pointer sizet)) (jmax (:pointer sizet)))
  :definition :methods
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

(defmfun mplusp ((a both))
  ("gsl_" :category :type "_ispos")
  (((mpointer a) :pointer))
  :definition :generic
  :gsl-version (1 9)
  :inputs (a)
  :c-return :boolean
  :documentation			; FDL
  "All elements of a are positive.")

(defmfun mminusp ((a both))
  ("gsl_" :category :type "_isneg")
  (((mpointer a) :pointer))
  :definition :generic
  :gsl-version (1 9)
  :inputs (a)
  :c-return :boolean
  :documentation			; FDL
  "All elements of a are negative.")

(defmfun non-negative-p ((a both))
  ("gsl_" :category :type "_isnonneg")
  (((mpointer a) :pointer))
  :definition :generic
  :gsl-version (1 10)
  :inputs (a)
  :c-return :boolean
  :documentation			; FDL
  "All elements of a are non-negative.")
