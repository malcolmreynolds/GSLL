;********************************************************
; file:        vector-matrix.lisp                        
; description: Vectors and matrices                      
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Wed Mar 29 2006 - 23:35
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
(cffi:defcstruct gsl-vector-c
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

(setf *wrap-types* (acons 'gsl-vector-c (lambda (x) `(pointer ,x)) *wrap-types*))

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
  (vector gsl-vector-c))

(setf *wrap-types*
      (acons 'gsl-vector-view
	     (lambda (x)
	       ;;x
	       #+no
	       `(cffi:foreign-slot-value ,x 'gsl-vector-view 'vector)
	       `(make-instance 'gsl-vector
		 :pointer
		 (cffi:foreign-slot-value ,x 'gsl-vector-view 'vector)))
	     *wrap-types*))

;;; improve documentation
(defun-gsl subvector ((vector gsl-vector-c) (offset :size) (size :size))
  "gsl_vector_subvector"
  :c-return-value :return
  :return (gsl-vector-view)
  :documentation
  "Return a vector view of a subvector of another vector
@var{v}.  The start of the new vector is offset by @var{offset} elements
from the start of the original vector.  The new vector has @var{size}
elements.")

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
(with-data (vec vector 3)
  (setf (gsl-aref vec 0) -3.21d0
	(gsl-aref vec 1) 1.0d0
	(gsl-aref vec 2) 12.8d0)
  (print (gsl-aref vec 2))
  (print (gsl-aref vec 1))
  (print (gsl-aref vec 0)))

(with-data (vec vector 3)
  (set-all vec 77.8d0)
  (print (gsl-aref vec 2))
  (print (gsl-aref vec 1))
  (print (gsl-aref vec 0))
  (set-zero vec)
  (print (gsl-aref vec 2))
  (print (gsl-aref vec 1))
  (print (gsl-aref vec 0)))

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

|#

