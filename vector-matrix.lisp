;********************************************************
; file:        vector-matrix.lisp                        
; description: Vectors and matrices                      
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Sun Mar 26 2006 - 18:18
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; No mechanism for C stream input/output yet.
;;; Generalize check-gsl-status to optionally signal errors, use here?
;;; Functions like write-binary etc. as a single function, selecting the C fn with typecase?

;;; Prospective syntax
;;; (setf (gsl-aref vec 0) -3.21d0)
;;; (gsl-aref vec 0)
;;; (gsl+ vec1 vec2)
;;; (with-data ((vec1 vector 3) (vec2 vector 3)) ...)

;;;;****************************************************************************
;;;; Blocks
;;;;****************************************************************************

(defclass gsl-block (data)
  ()
  (:documentation "GSL block."))

;;; Block definition
(cffi:defcstruct block
  (size :size)
  (data :pointer))

(gsl-data-functions "block")

;;;;****************************************************************************
;;;; Vectors
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

(defclass gsl-vector (data)
  ()
  (:documentation "GSL vector."))

(gsl-data-functions "vector")

(defun-gsl gsl-vector-get ((vector :pointer) (i :size))
  "gsl_vector_get"
  :return (:double)
  :c-return-value :return
  :documentation "The ith element of the vector.")

(defun-gsl gsl-vector-set ((vector :pointer) (i :size) (value :double))
  "gsl_vector_set"
  :c-return-value :void
  :documentation "Set the ith element of the vector.")

(defun-gsl gsl-vector-ptr ((vector :pointer) (i :size))
  "gsl_vector_ptr"
  :return (:pointer)
  :c-return-value :return
  :documentation "The ith element of the vector as a pointer.")

#+example
(with-data (vector vec 3)
  (gsl-vector-set vec 0 -3.21d0)
  (gsl-vector-set vec 1  1.0d0)
  (gsl-vector-set vec 2  12.8d0)
  (print (gsl-vector-get vec 2))
  (print (gsl-vector-get vec 1))
  (print (gsl-vector-get vec 0)))

;;; Is it possible to do
;;; (setf (gsl-aref vec 0) -3.21d0 (gsl-aref vec 1) 1.0d0)
;;; without defining objects?
;;; Inside with-data macro:
;;; (gsl-set vec 0 -3.21d0)
;;; Outside:
;;; (gsl-set vec 'vector 0 -3.21d0)

#+prototype
(defclass gsl-vector ()
  (pointer :initarg pointer :reader pointer)
  )

#+prototype
(defmethod gsl-aref ((ptr gsl-vector))
  (gsl-vector-get ptr))

