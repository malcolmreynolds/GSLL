;********************************************************
; file:        vector-matrix.lisp                        
; description: Vectors and matrices                      
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Mon Mar 27 2006 - 12:45
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; No mechanism for C stream input/output yet.
;;; Generalize check-gsl-status to optionally signal errors, use here?
;;; Functions like write-binary etc. as a single function, selecting the C fn with typecase?

;;; Syntax:
;;; (setf (gsl-aref vec 0) -3.21d0)
;;; (gsl-aref vec 0)
;;; (gsl+ vec1 vec2)
;;; (with-data ((vec1 vector 3) (vec2 vector 3)) ...)

;;;;****************************************************************************
;;;; Vector object definition, allocation, reading & writing
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
(cffi:defcstruct gsl-vector
  (size :size)
  (stride :size)
  (data :pointer)
  (block :pointer)
  (owner :int))

(defclass gsl-vector (gsl-data)
  ()
  (:documentation "GSL vector."))

;;; Allocation, freeing, reading and writing
(gsl-data-functions "vector")

;;;;****************************************************************************
;;;; Accessing elements
;;;;****************************************************************************

(defun-gsl gsl-vector-get ((vector :pointer) (i :size))
  "gsl_vector_get"
  :return (:double)
  :c-return-value :return
  :documentation "The ith element of the vector.")

(defmethod gsl-aref ((object gsl-vector) &rest indices)
  (gsl-vector-get (pointer object) (first indices)))

(defun-gsl gsl-vector-set ((vector :pointer) (i :size) (value :double))
  "gsl_vector_set"
  :c-return-value :void
  :documentation "Set the ith element of the vector.")

(defmethod (setf gsl-aref) (value (object gsl-vector) &rest indices)
  (gsl-vector-set (pointer object) (first indices) value))

(defun-gsl gsl-vector-ptr ((vector :pointer) (i :size))
  "gsl_vector_ptr"
  :return (:pointer)
  :c-return-value :return
  :documentation "The ith element of the vector as a pointer.")

;;;;****************************************************************************
;;;; Initializing elements
;;;;****************************************************************************

(defmethod set-all ((object gsl-vector) value)
  (funcall
   (defun-gsl :lambda ((pointer :pointer) (value :double))
     "gsl_vector_set_all"
     :return ()
     :c-return-value :void)
   (pointer object)
   value))

(defmethod set-zero ((object gsl-vector))
  (funcall
   (defun-gsl :lambda ((pointer :pointer))
     "gsl_vector_set_zero"
     :return ()
     :c-return-value :void)
   (pointer object)))

;;; maybe map inputs in defun-gsl?
(defunx set-basis (object index)
  (funcall
   (defun-gsl :lambda ((pointer :pointer) (index :size))
     "gsl_vector_set_basis"
     :return ())
   (pointer object)
   index))

;;;;****************************************************************************
;;;; Views
;;;;****************************************************************************

(cffi:defcstruct gsl-vector-view
  (vector gsl-vector))

(defun-gsl subvector ((vector gsl-vector) (offset :size) (size :size))
  "gsl_vector_subvector"
  :c-return-value :return
  :return (gsl-vector-view)
  :documentation
  "Return a vector view of a subvector of another vector
@var{v}.  The start of the new vector is offset by @var{offset} elements
from the start of the original vector.  The new vector has @var{size}
elements.  Mathematically, the @var{i}-th element of the new vector
@var{v'} is given by,

@example
v'(i) = v->data[(offset + i)*v->stride]
@end example
@noindent
where the index @var{i} runs from 0 to @code{n-1}.

The @code{data} pointer of the returned vector struct is set to null if
the combined parameters (@var{offset},@var{n}) overrun the end of the
original vector.????????????

The new vector is only a view of the block underlying the original
vector, @var{v}.  The block containing the elements of @var{v} is not
owned by the new vector.  When the view goes out of scope the original
vector @var{v} and its block will continue to exist.  The original
memory can only be deallocated by freeing the original vector.  Of
course, the original vector should not be deallocated while the view is
still in use.")

;;;;****************************************************************************
;;;; Copying
;;;;****************************************************************************

;;;;****************************************************************************
;;;; Exchanging elements
;;;;****************************************************************************

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

#|
(with-data (vector vec 3)
  (setf (gsl-aref vec 0) -3.21d0
	(gsl-aref vec 1) 1.0d0
	(gsl-aref vec 2) 12.8d0)
  (print (gsl-aref vec 2))
  (print (gsl-aref vec 1))
  (print (gsl-aref vec 0)))

(with-data (vector vec 3)
  (set-all vec 77.8d0)
  (print (gsl-aref vec 2))
  (print (gsl-aref vec 1))
  (print (gsl-aref vec 0))
  (set-zero vec)
  (print (gsl-aref vec 2))
  (print (gsl-aref vec 1))
  (print (gsl-aref vec 0)))

(with-data (vector vec 3) (set-basis vec 1)
	   (format t "~&~a ~a ~a"
		   (gsl-aref vec 0) (gsl-aref vec 1) (gsl-aref vec 2)))

|#

