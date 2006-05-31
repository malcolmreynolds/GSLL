;********************************************************
; file:        vector.lisp                        
; description: Vectors
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Wed May 31 2006 - 09:33
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; No mechanism for C stream input/output yet.
;;; Generalize check-gsl-status to optionally signal errors, use here?
;;; Functions like write-binary etc. as a single function, selecting the C fn with typecase?
;;; #'subvector, #'subvector-stride cause crash, see notes 2006-03-30
;;; #'vector-complex-real, #'vector-complex-imag need structure definition

;;; Need to build real vector out of view pointer.

;;; Syntax:
;;; (setf (gsl-aref vec 0) -3.21d0)
;;; (gsl-aref vec 0)
;;; (gsl+ vec1 vec2)
;;; (with-data ((vec1 vector 3) (vec2 vector 3)) ...)

;;;;****************************************************************************
;;;; Vector structure and CL object
;;;;****************************************************************************

#|
The @var{size} is simply the number of vector elements.  The range of
valid indices runs from 0 to @code{size-1}.  The @var{stride} is the
step-size from one element to the next in physical memory, measured in
units of the appropriate datatype.  The pointer @var{data} gives the
location of the first element of the vector in memory.  The pointer
@var{block} stores the location of the memory block in which the vector
elements are located (if any).  If the vector owns this block then the
@var{owner} field is set to one and the block will be deallocated when the
vector is freed.  If the vector points to a block owned by another
object then the @var{owner} field is zero and any underlying block will not be
deallocated with the vector.
|#

;;; GSL-vector definition
(cffi:defcstruct gsl-vector-c
  (size :size)
  (stride :size)
  (data :pointer)
  (block :pointer)
  (owner :int))

;;; Allocation, freeing, reading and writing
(defdata "vector" vector-double double-float)
(defdata "vector_float" vector-single single-float)
(defdata "vector_complex" vector-complex complex)
(defdata "vector_int" vector-fixnum fixnum)

(defmethod gsl-array ((object gsl-vector))
  (foreign-slot-value (pointer object) 'gsl-vector-c 'data))

;;;;****************************************************************************
;;;; Getting values
;;;;****************************************************************************

(defun-gsl gsl-aref ((vector gsl-vector) &rest indices)
    "gsl_vector_get"
  (((pointer vector) :pointer) ((first indices) :size))
  :type :method
  :c-return :double
  :documentation "The ith element of the vector.")

(defun-gsl gsl-vector-ptr (vector i)
  "gsl_vector_ptr" (((pointer vector) :pointer) (i :size))
  :c-return :pointer
  :documentation "The ith element of the vector as a pointer.")

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defun-gsl (setf gsl-aref) (value (vector gsl-vector) &rest indices)
  "gsl_vector_set"
  (((pointer vector) :pointer) ((first indices) :size) (value :double))
  :type :method
  :c-return :void
  :documentation "Set the ith element of the vector.")

(defun-gsl set-all ((object gsl-vector) value)
  "gsl_vector_set_all"
  (((pointer object) :pointer) (value :double))
  :type :method 
  :c-return :void)

(defun-gsl set-zero ((object gsl-vector))
  "gsl_vector_set_zero" (((pointer object) :pointer))
  :type :method
  :c-return :void)

(defun-gsl set-basis (vector index)
  "gsl_vector_set_basis" (((pointer vector) gsl-vector-c) (index :size))
  :documentation "Set the index element to 1.0, and the rest to 0.0."
  :invalidate (vector)
  :return (vector))

;;;;****************************************************************************
;;;; Views
;;;;****************************************************************************

(cffi:defcstruct gsl-vector-view
  (vector gsl-vector-c))

(defun-gsl subvector (vector offset size)
    "gsl_vector_subvector"
  ((vector gsl-vector-c) (offset :size) (size :size))
  :c-return :pointer
  :documentation
  "Return a vector view of a subvector of another vector
  @var{v}.  The start of the new vector is offset by @var{offset} elements
  from the start of the original vector.  The new vector has @var{size}
  elements.")

(defun-gsl subvector-stride (vector offset stride size)
  "gsl_vector_subvector_with_stride"
  ((vector gsl-vector-c) (offset :size) (stride :size) (size :size))
  :c-return :pointer
  :documentation
  "A vector view of a subvector of another vector
  @var{v} with an additional stride argument. The subvector is formed in
  the same way as for @code{gsl_vector_subvector} but the new vector has
  @var{n} elements with a step-size of @var{stride} from one element to
  the next in the original vector.  Mathematically, the @var{i}-th element
  of the new vector @var{v'} is given by
  v'(i) = v->data[(offset + i*stride)*v->stride]
  where the index @var{i} runs from 0 to @code{n-1}.
  Note that subvector views give direct access to the underlying elements
  of the original vector.")

;;; These require that the gsl-vector-complex structure be defined.
#|
(defun-gsl vector-complex-real (vector)
  "gsl_vector_complex_real" ((vector gsl-vector-complex))
  :c-return :pointer
  :documentation
  "A vector view of the real parts of the complex vector @var{v}.")

(defun-gsl vector-complex-imag (vector)
  "gsl_vector_complex_imag"((vector gsl-vector-complex))
  :c-return :pointer
  :documentation
  "A vector view of the imaginary parts of the complex vector @var{v}.")
|#

(defun-gsl vector-array (base size)
  "gsl_vector_view_array" ((base :pointer) (size :size))
  :c-return :pointer
  :documentation
  "A vector view of an array.  The start of the new
  vector is given by @var{base} and has @var{n} elements.")

(defun-gsl vector-array-stride (base stride size)
  "gsl_vector_view_array_with_stride"
  ((base :pointer) (stride :size) (size :size))
  :c-return :pointer
  :documentation
  "A vector view of an array with stride.  The start of the new
  vector is given by @var{base}.")

;;;;****************************************************************************
;;;; Copying
;;;;****************************************************************************

(defun-gsl vector-copy (destination source)
  "gsl_vector_memcpy"
  (((pointer destination) gsl-vector-c) ((pointer source) gsl-vector-c))
  :invalidate (destination)
  :return (destination)
  :documentation
  "Copy the elements of the vector @var{source} into the
   vector @var{destination}.  The two vectors must have the same length.")

(defun-gsl vector-swap (v w)
  "gsl_vector_swap" (((pointer v) gsl-vector-c) ((pointer w) gsl-vector-c))
  :invalidate (v w)
  :return (v w)
  :documentation
  "Exchange the elements of the vectors @var{v} and @var{w}
   by copying.  The two vectors must have the same length.")

;;;;****************************************************************************
;;;; Exchanging elements
;;;;****************************************************************************

(defun-gsl vector-swap-elements (vec i j)
  "gsl_vector_swap_elements" (((pointer vec) gsl-vector-c) (i :size) (j :size))
  :after ((push (list i) (cl-invalid vec))
	  (push (list j) (cl-invalid vec)))
  :return (vec)
  :documentation
  "Exchange the @var{i}-th and @var{j}-th elements of the
   vector @var{vec} in-place.")

(defun-gsl vector-reverse (vec)
  "gsl_vector_reverse" (((pointer vec) gsl-vector-c))
  :invalidate (vec)
  :documentation
  "Reverse the order of the elements of the vector @var{vec}.")

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(defun-gsl vector+ (a b)
  "gsl_vector_add" (((pointer a) gsl-vector-c) ((pointer b) gsl-vector-c))
  :invalidate (a)
  :documentation
  "Add the elements of vector @var{b} to the elements of
  vector @var{a}, @math{a'_i = a_i + b_i}. The two vectors must have the
  same length.")

(defun-gsl vector- (a b)
  "gsl_vector_sub" (((pointer a) gsl-vector-c) ((pointer b) gsl-vector-c))
  :invalidate (a)
  :documentation
  "Subtract the elements of vector @var{b} from the elements of
vector @var{a}, @math{a'_i = a_i - b_i}. The two vectors must have the
same length.")

(defun-gsl vector* (a b)
  "gsl_vector_mul" (((pointer a) gsl-vector-c) ((pointer b) gsl-vector-c))
  :invalidate (a)
  :documentation
  "Multiply the elements of vector @var{a} by the elements of
  vector @var{b}, @math{a'_i = a_i * b_i}. The two vectors must have the
  same length.")

(defun-gsl vector/ (a b)
  "gsl_vector_div" (((pointer a) gsl-vector-c) ((pointer b) gsl-vector-c))
  :invalidate (a)
  :documentation
  "Divide the elements of vector @var{a} by the elements of
  vector @var{b}, @math{a'_i = a_i / b_i}. The two vectors must have the
  same length.")

(defun-gsl vector*c (a x)
  "gsl_vector_scale" (((pointer a) gsl-vector-c) (x :double))
  :invalidate (a)
  :documentation
  "Multiply the elements of vector @var{a} by the constant
  factor @var{x}, @math{a'_i = x a_i}.")

(defun-gsl vector+c (a x)
  "gsl_vector_add_constant" (((pointer a) gsl-vector-c) (x :double))
  :invalidate (a)
  :documentation
  "Add the constant value @var{x} to the elements of the
  vector @var{a}, @math{a'_i = a_i + x}.")

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(defun-gsl vector-max (v)
  "gsl_vector_max" (((pointer v) gsl-vector-c))
  :documentation
  "The maximum value in the vector @var{v}."
  :c-return :double)

(defun-gsl vector-min (v)
  "gsl_vector_min" (((pointer v) gsl-vector-c))
  :documentation
  "The minimum value in the vector @var{v}."
  :c-return :double)

(defun-gsl vector-minmax (v)
  "gsl_vector_minmax" (((pointer v) gsl-vector-c) (min :double) (max :double))
  :documentation
  "The minimum and maximum values in the vector @var{v}."
  :c-return :void)

(defun-gsl vector-max-index (v)
  "gsl_vector_max_index" (((pointer v) gsl-vector-c))
  :documentation
  "The index of the maximum value in the vector @var{v}.
   When there are several equal minimum elements then the lowest index is
   returned."
  :c-return :size)

(defun-gsl vector-min-index (v)
  "gsl_vector_min_index" (((pointer v) gsl-vector-c))
  :documentation
  "The index of the minimum value in the vector @var{v}.  When there are several
  equal minimum elements then the lowest index is returned."
  :c-return :size)

(defun-gsl vector-minmax-index (v)
  "gsl_vector_minmax_index"
  (((pointer v) gsl-vector-c) (imin :size) (imax :size))
  :documentation
  "The indices of the minimum and maximum values in the vector @var{v}.
  When there are several equal minimum elements then the lowest index is
  returned."
  :c-return :void)

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

(defun-gsl vector-zerop (v)
  "gsl_vector_isnull" (((pointer v) gsl-vector-c))
  :documentation
  "All elements of vector @var{v} are zero."
  :c-return :boolean)

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

#|
(defun print-vector (vec)
  (format t "~&~f ~f ~f"
	  (gsl-aref vec 0) (gsl-aref vec 1) (gsl-aref vec 2)))

(defun set-vector (vec &rest values)
  (setf (gsl-aref vec 0) (nth 0 values)
	(gsl-aref vec 1) (nth 1 values)
	(gsl-aref vec 2) (nth 2 values)))

(with-data (vec vector 3)
  (setf (gsl-aref vec 0) -3.21d0
	(gsl-aref vec 1) 1.0d0
	(gsl-aref vec 2) 12.8d0)
  (print-vector vec))

(with-data (vec vector 3)
  (setf (data vec) #(-3.21d0 1.0d0 12.8d0))
  (print-vector vec))

(defparameter vec (make-data 'vector nil 3))
(setf (data vec) #(-3.21d0 1.0d0 12.8d0))
(free vec)


(with-data (vec vector 3)
  (setf (data vec) #(-3.21d0 1.0d0 12.8d0))
  (data vec))

(with-data (vec vector 3)
  (set-all vec 77.8d0)
  (print-vector vec)
  (set-zero vec)
  (print-vector vec))

(with-data (vec vector 3) (set-basis vec 1)
	   (format t "~&~a ~a ~a"
		   (gsl-aref vec 0) (gsl-aref vec 1) (gsl-aref vec 2)))

;;; broken
(with-data (vec vector 3)
  (setf (gsl-aref vec 0) -3.21d0
	(gsl-aref vec 1) 1.0d0
	(gsl-aref vec 2) 12.8d0)
  (subvector vec 1 2))

(DEFUN SUBVECTOR (VECTOR OFFSET SIZE)
  (FOREIGN-FUNCALL "gsl_vector_subvector"
			  :pointer
			  (POINTER VECTOR)
			  :SIZE
			  OFFSET
			  :SIZE
			  SIZE
			  GSL-VECTOR-VIEW))

(with-data (vec vector 3)
  (setf (gsl-aref vec 0) -3.21d0
	(gsl-aref vec 1) 1.0d0
	(gsl-aref vec 2) 12.8d0)
  (FOREIGN-FUNCALL "gsl_vector_subvector"
			  :pointer
			  (POINTER VEC)
			  :SIZE
			  1
			  :SIZE
			  2
			  GSL-VECTOR-VIEW))

(with-data (vec1 vector 3)
  (with-data (vec2 vector 3)
    (setf (gsl-aref vec1 0) -3.21d0
	  (gsl-aref vec1 1) 1.0d0
	  (gsl-aref vec1 2) 12.8d0)
    (vector-copy vec2 vec1)
    (print-vector vec2)))

(with-data (vec1 vector 3)
  (set-vector vec1 -3.21d0 1.0d0 12.8d0) (print-vector vec1)
  (vector-swap-elements vec1 0 1) (print-vector vec1)
  (vector-reverse vec1) (print-vector vec1)
  (vector*c vec1 2.0d0) (print-vector vec1)
  (vector+c vec1 2.0d0) (print-vector vec1))

(with-data (vec1 vector 3)
  (with-data (vec2 vector 3)
    (set-vector vec1 -3.21d0 1.0d0 12.8d0)
    (set-vector vec2 1.4d0 17.0d0 -3.0d0)
    (vector+ vec1 vec2)
    (print-vector vec1)
    (vector* vec1 vec2)
    (print-vector vec1)))

(with-data (vec1 vector 3)
  (set-vector vec1 -3.21d0 1.0d0 12.8d0)
  (print-vector vec1)
  (print (vector-min vec1))
  (print (vector-max vec1))
  (vector-minmax vec1))

(with-data (vec1 vector 3)
  (set-vector vec1 -3.21d0 1.0d0 12.8d0)
  (print-vector vec1)
  (print (vector-min-index vec1))
  (print (vector-max-index vec1))
  (vector-minmax-index vec1))

|#
