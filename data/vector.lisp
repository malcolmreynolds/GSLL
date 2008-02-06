;; Vectors
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2008-02-04 19:44:24EST vector.lisp>
;; $Id: $

(in-package :gsl)

;;; Vectors are specified in a letm binding with
;;;  (vector-double size-or-initial &optional zero)
;;;  (vector-single size-or-initial &optional zero)
;;;  (vector-fixnum size-or-initial &optional zero)
;;;  (vector-complex size-or-initial &optional zero)
;;; where size-or-initial is a positive integer indicating the
;;; size, and zero indicates that all elements should be set to zero,
;;; or, size-or-initial is a sequence to which the vector
;;; should be initially set.

;;; No mechanism for C stream input/output yet.
;;; Generalize check-gsl-status to optionally signal errors, use here?
;;; Functions like write-binary etc. as a single function, selecting the C fn with typecase?
;;; #'subvector, #'subvector-stride cause crash, see notes 2006-03-30
;;; #'vector-complex-real, #'vector-complex-imag need structure definition

;;; GSL bug?:  no gsl_vector_complex_add, etc.

;;; Need to build real vector out of view pointer.

;;; Need to #'cl-invalidate when (setf gsl-aref) called, see Mon Nov 26 2007.


;;;;****************************************************************************
;;;; Vector structure and CL object
;;;;****************************************************************************

#|
;;FDL
The size is simply the number of vector elements.  The range of
valid indices runs from 0 to size-1.  The stride is the
step-size from one element to the next in physical memory, measured in
units of the appropriate datatype.  The pointer data gives the
location of the first element of the vector in memory.  The pointer
block stores the location of the memory block in which the vector
elements are located (if any).  If the vector owns this block then the
owner field is set to one and the block will be deallocated when the
vector is freed.  If the vector points to a block owned by another
object then the owner field is zero and any underlying block will not be
deallocated with the vector.
|#

;;; GSL-vector definition
(cffi:defcstruct gsl-vector-c
  (size :size)
  (stride :size)
  (data :pointer)
  (block :pointer)
  (owner :int))

(defclass gsl-vector (gsl-data) ())

;;; Allocation, freeing, reading and writing
(defdata "vector" vector-double double-float gsl-vector)
(defdata "vector_float" vector-single single-float gsl-vector)
(defdata "vector_int" vector-fixnum fixnum gsl-vector)
(defdata "vector_complex" vector-complex complex gsl-vector)
;;; (defdata "vector_uint" vector-unsigned-fixnum (unsigned-byte 64) gsl-vector)
;;; doesn't work 2007-03-04.

(defmacro defun-gsl-vdsfc (&rest args)
  "A defun-gsl for vectors of double, single, fixnum, and complex."
  (defun-gsl-all
      '(double single fixnum complex)
      '(:double :float :int gsl-complex)
    "vector"
    'gsl-vector
    args))

(defmacro defun-gsl-vdsf (&rest args)
  "A defun-gsl for vectors of double, single, and fixnum."
  (defun-gsl-all
      '(double single fixnum)
      '(:double :float :int)
    "vector"
    'gsl-vector
    args))

(defmethod gsl-array ((object gsl-vector))
  (cffi:foreign-slot-value (pointer object) 'gsl-vector-c 'data))

(defun make-data-from-pointer (pointer &optional (class 'gsl-vector-double) size)
  "Given a C pointer to a GSL data type, make the CL object."
  (make-instance
   class
   :pointer pointer
   :storage-size
   (or size (cffi:foreign-slot-value pointer 'gsl-vector-c 'size))))

(export 'vector-data)
(defun vector-data (pointer)
  "A pointer to the GSL array with the data contents, from the
   sruct pointer."
  (cffi:foreign-slot-value pointer 'gsl-vector-c 'data))

;;;;****************************************************************************
;;;; Getting values
;;;;****************************************************************************

(defun-gsl-vdsfc gsl-aref ((vector gsl-vector) &rest indices)
    "gsl_vector_get"
  (((pointer vector) :pointer) ((first indices) :size))
  :c-return :c-base-type
  :documentation			; FDL
  "The ith element of the vector.")

(defun-gsl vref (pointer index)
  "gsl_vector_get"
  ((pointer :pointer) (index :size))
  :c-return :double
  :index nil
  :documentation "An element of the vector of doubles, computed from the pointer.")

(defun-gsl gsl-vector-ptr (vector i)
  "gsl_vector_ptr" (((pointer vector) :pointer) (i :size))
  :c-return :pointer
  :documentation			; FDL
  "The ith element of the vector as a pointer.")

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defun-gsl-vdsfc (setf gsl-aref) (value (vector gsl-vector) &rest indices)
  "gsl_vector_set"
  (((pointer vector) :pointer) ((first indices) :size) (value :c-base-type))
  :c-return :void
  :documentation			; FDL
  "Set an element of the vector.")

(defun-gsl (setf vref) (value pointer index)
  "gsl_vector_set"
  ((pointer :pointer) (index :size) (value :double))
  :c-return :void
  :index nil
  :documentation			; FDL
 "Set an element of the vector of doubles, using its pointer.")

(defun-gsl-vdsfc set-all ((object gsl-vector) value)
  "gsl_vector_set_all"
  (((pointer object) :pointer) (value :c-base-type))
  :c-return :void)

(defun-gsl-vdsfc set-zero ((object gsl-vector))
  "gsl_vector_set_zero" (((pointer object) :pointer))
  :c-return :void)

(defun-gsl-vdsfc set-basis ((vector gsl-vector) index)
  "gsl_vector_set_basis" (((pointer vector) gsl-vector-c) (index :size))
  :invalidate (vector)
  :documentation			; FDL
  "Set the index element to 1, and the rest to 0.")

;;;;****************************************************************************
;;;; Views
;;;;****************************************************************************

(cffi:defcstruct gsl-vector-view
  (vector gsl-vector-c))

;;; broken
;;; (letm ((vec (vector-double  #(-3.21d0 1.0d0 12.8d0)))) (subvector vec 1 2))

(export '(subvector subvector-stride))

(defgeneric subvector (vector offset size)
  (:documentation			; FDL
   "Return a vector view of a subvector of another vector
  v.  The start of the new vector is offset by offset elements
  from the start of the original vector.  The new vector has size
  elements."))

(defgeneric subvector-stride (vector offset stride size)
  (:documentation			; FDL
   "A vector view of a subvector of another vector
  v with an additional stride argument. The subvector is formed in
  the same way as for #'subvector but the new vector has
  n elements with a step-size of stride from one element to
  the next in the original vector.  Mathematically, the i-th element
  of the new vector v' is given by
  v'(i) = v->data[(offset + i*stride)*v->stride]
  where the index i runs from 0 to n-1.
  Note that subvector views give direct access to the underlying elements
  of the original vector."))

(defun-gsl-vdsfc subvector ((vector gsl-vector) offset size)
    "gsl_vector_subvector"
  (((pointer vector) gsl-vector-c) (offset :size) (size :size))
  :c-return :pointer)

(defun-gsl-vdsfc subvector-stride ((vector gsl-vector) offset stride size)
  "gsl_vector_subvector_with_stride"
  (((pointer vector) gsl-vector-c)
   (offset :size) (stride :size) (size :size))
  :c-return :pointer)

;;; These require that the gsl-vector-complex structure be defined.
#|
(defun-gsl vector-complex-real (vector)
  "gsl_vector_complex_real" ((vector gsl-vector-complex))
  :c-return :pointer
  :documentation
  "A vector view of the real parts of the complex vector v.")

(defun-gsl vector-complex-imag (vector)
  "gsl_vector_complex_imag"((vector gsl-vector-complex))
  :c-return :pointer
  :documentation
  "A vector view of the imaginary parts of the complex vector v.")
|#

(defun-gsl vector-array (base size)
  "gsl_vector_view_array" ((base :pointer) (size :size))
  :c-return :pointer
  :documentation			; FDL
  "A vector view of an array.  The start of the new
  vector is given by base and has n elements.")

(defun-gsl vector-array-stride (base stride size)
  "gsl_vector_view_array_with_stride"
  ((base :pointer) (stride :size) (size :size))
  :c-return :pointer
  :documentation			; FDL
  "A vector view of an array with stride.  The start of the new
  vector is given by base.")

;;;;****************************************************************************
;;;; Copying
;;;;****************************************************************************

(defun-gsl-vdsfc copy ((destination gsl-vector) (source gsl-vector))
  "gsl_vector_memcpy"
  (((pointer destination) gsl-vector-c) ((pointer source) gsl-vector-c))
  :invalidate (destination)
  :documentation			; FDL
  "Copy the elements of the vector source into the
   vector destination.  The two vectors must have the same length.")

(defun-gsl-vdsfc swap ((v gsl-vector) (w gsl-vector))
  "gsl_vector_swap" (((pointer v) gsl-vector-c) ((pointer w) gsl-vector-c))
  :invalidate (v w)
  :documentation			; FDL
  "Exchange the elements of the vectors v and w
   by copying.  The two vectors must have the same length.")

;;;;****************************************************************************
;;;; Exchanging elements
;;;;****************************************************************************

(export '(swap-elements vector-reverse))

(defgeneric swap-elements (vec i j)
  (:documentation			; FDL
   "Exchange the i-th and j-th elements of the vector vec in-place."))

(defgeneric vector-reverse (vec)
  (:documentation
   "Exchange the i-th and j-th elements of the vector vec in-place."))

(defun-gsl-vdsfc swap-elements ((vec gsl-vector) i j)
  "gsl_vector_swap_elements" (((pointer vec) gsl-vector-c) (i :size) (j :size))
  :after ((when (listp (cl-invalid vec))
	    (push (list i) (cl-invalid vec))
	    (push (list j) (cl-invalid vec))))
  :return (vec))

(defun-gsl-vdsfc vector-reverse ((vec gsl-vector))
  "gsl_vector_reverse" (((pointer vec) gsl-vector-c))
  :invalidate (vec)
  :documentation			; FDL
  "Reverse the order of the elements of the vector vec.")

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(defun-gsl-vdsf gsl+ ((a gsl-vector) (b gsl-vector))
  "gsl_vector_add" (((pointer a) gsl-vector-c) ((pointer b) gsl-vector-c))
  :invalidate (a)
  :documentation			; FDL
  "Add the elements of vector b to the elements of
  vector a, a'_i = a_i + b_i. The two vectors must have the
  same length.")

(defun-gsl-vdsf gsl- ((a gsl-vector) (b gsl-vector))
  "gsl_vector_sub" (((pointer a) gsl-vector-c) ((pointer b) gsl-vector-c))
  :invalidate (a)
  :documentation			; FDL
  "Subtract the elements of vector b from the elements of
   vector a, a'_i = a_i - b_i.  The two vectors must have the same length.")

(defun-gsl-vdsf gsl* ((a gsl-vector) (b gsl-vector))
  "gsl_vector_mul" (((pointer a) gsl-vector-c) ((pointer b) gsl-vector-c))
  :invalidate (a)
  :documentation			; FDL
  "Multiply the elements of vector a by the elements of
  vector b, a'_i = a_i * b_i. The two vectors must have the same length.")

(defun-gsl-vdsf gsl/ ((a gsl-vector) (b gsl-vector))
  "gsl_vector_div" (((pointer a) gsl-vector-c) ((pointer b) gsl-vector-c))
  :invalidate (a)
  :documentation			; FDL
  "Divide the elements of vector a by the elements of
  vector b, a'_i = a_i / b_i. The two vectors must have the same length.")

(defun-gsl-vdsf gsl*c ((a gsl-vector) x)
  "gsl_vector_scale" (((pointer a) gsl-vector-c) (x :double))
  :invalidate (a)
  :documentation			; FDL
  "Multiply the elements of vector a by the constant factor x, a'_i = x a_i.")

(defun-gsl-vdsf gsl+c ((a gsl-vector) x)
  "gsl_vector_add_constant" (((pointer a) gsl-vector-c) (x :double))
  :invalidate (a)
  :documentation			; FDL
  "Add the constant value x to the elements of the vector a, a'_i = a_i + x.")

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(defun-gsl-vdsf gsl-max ((v gsl-vector))
  "gsl_vector_max" (((pointer v) gsl-vector-c))
  :c-return :c-base-type
  :documentation			; FDL
  "The maximum value in the vector v.")

(defun-gsl-vdsf gsl-min ((v gsl-vector))
  "gsl_vector_min" (((pointer v) gsl-vector-c))
  :c-return :c-base-type
  :documentation			; FDL
  "The minimum value in the vector v.")

(defun-gsl-vdsf gsl-minmax ((v gsl-vector))
  "gsl_vector_minmax"
  (((pointer v) gsl-vector-c) (min :c-base-type) (max :c-base-type))
  :c-return :void
  :documentation			; FDL
  "The minimum and maximum values in the vector v.")

(defun-gsl-vdsf gsl-max-index ((v gsl-vector))
  "gsl_vector_max_index" (((pointer v) gsl-vector-c))
  :c-return :size
  :documentation			; FDL
  "The index of the maximum value in the vector v.
   When there are several equal minimum elements then the lowest index is
   returned.")

(defun-gsl-vdsf gsl-min-index ((v gsl-vector))
  "gsl_vector_min_index" (((pointer v) gsl-vector-c))
  :c-return :size
  :documentation			; FDL
  "The index of the minimum value in the vector v.  When there are several
  equal minimum elements then the lowest index is returned.")

(defun-gsl-vdsf gsl-minmax-index ((v gsl-vector))
  "gsl_vector_minmax_index"
  (((pointer v) gsl-vector-c) (imin :size) (imax :size))
  :c-return :void
  :documentation			; FDL
  "The indices of the minimum and maximum values in the vector v.
  When there are several equal minimum elements then the lowest index is
  returned.")

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

(defun-gsl-vdsfc gsl-zerop ((v gsl-vector))
  "gsl_vector_isnull" (((pointer v) gsl-vector-c))
  :c-return :boolean
  :documentation			; FDL
  "All elements of vector v are zero.")

;;;;****************************************************************************
;;;; Examples and unit tests
;;;;****************************************************************************

(lisp-unit:define-test vector-fixnum
  (lisp-unit:assert-eql			;(setf gsl-aref), gsl-aref
   77
   (letm ((intvec (vector-fixnum 4)))
     (setf (gsl-aref intvec 1) 77)
     (gsl-aref intvec 1)))
  (lisp-unit:assert-equalp		;(setf data)
   #(4 6 8 2)
   (letm ((intvec (vector-fixnum 4)))
     (setf (data intvec) #(4 6 8 2))
     (data intvec)))
  (lisp-unit:assert-equalp		;set-zero
   #(0 0 0 0)
   (letm ((intvec (vector-fixnum 4)))
     (set-zero intvec)
     (data intvec)))
  (lisp-unit:assert-equalp		;set-all
   #(44 44 44 44)
   (letm ((intvec (vector-fixnum 4)))
     (set-all intvec 44)
     (data intvec)))
  (lisp-unit:assert-equalp		;set-basis
   #(0 1 0 0)
   (letm ((intvec (vector-fixnum 4)))
     (set-basis intvec 1)
     (data intvec)))
  (lisp-unit:assert-equalp		;vector-reverse
   #(4 3 2 1)
   (letm ((intvec (vector-fixnum #(1 2 3 4))))
     (vector-reverse intvec)
     (data intvec)))
  (lisp-unit:assert-eql			;gsl-min
   -12
   (letm ((intvec (vector-fixnum #(-1 -12 8 3))))
     (gsl-min intvec)))
  (lisp-unit:assert-eql			;gsl-max
   8
   (letm ((intvec (vector-fixnum #(-1 -12 8 3))))
     (gsl-max intvec)))
  (lisp-unit:assert-equal		;gsl-minmax
   '(-12 8)
   (letm ((intvec (vector-fixnum #(-1 -12 8 3))))
     (multiple-value-list (gsl-minmax intvec))))
  (lisp-unit:assert-eql			;gsl-min-index
   1
   (letm ((intvec (vector-fixnum #(-1 -12 8 3))))
     (gsl-min-index intvec)))
  (lisp-unit:assert-eql			;gsl-max-index
   2
   (letm ((intvec (vector-fixnum #(-1 -12 8 3))))
     (gsl-max-index intvec)))
  (lisp-unit:assert-equal		;gsl-minmax-index
   '(1 2)
   (letm ((intvec (vector-fixnum #(-1 -12 8 3))))
     (multiple-value-list (gsl-minmax-index intvec))))
  (lisp-unit:assert-equalp		;copy
   #(1 2 3 4)
   (letm ((intvec1 (vector-fixnum #(1 2 3 4)))
	  (intvec2 (vector-fixnum 4)))
     (copy intvec2 intvec1)
     (data intvec2)))
  (lisp-unit:assert-equalp		;swap
   #(5 6 7 8 1 2 3 4)
   (letm ((intvec1 (vector-fixnum #(1 2 3 4)))
	  (intvec2 (vector-fixnum #(5 6 7 8))))
     (swap intvec2 intvec1)
     (concatenate 'vector (data intvec1) (data intvec2))))
  (lisp-unit:assert-equalp		;swap-elements
   #(1 4 3 2)
   (letm ((intvec (vector-fixnum #(1 2 3 4))))
     (swap-elements intvec 1 3)
     (data intvec))))

(lisp-unit:define-test vector-double
  (lisp-unit:assert-equal
   '("-0.321000000000d+01" "0.100000000000d+01" "0.128000000000d+02")
   (lisp-unit:fp-sequence 
    (letm ((vec (vector-double 3)))
      (setf (gsl-aref vec 0) -3.21d0
	    (gsl-aref vec 1) 1.0d0
	    (gsl-aref vec 2) 12.8d0
	    (cl-invalid vec) t)
      (data vec)))
   (lisp-unit:assert-equal
    '("-0.321000000000d+01" "0.100000000000d+01" "0.128000000000d+02")
    (lisp-unit:fp-sequence 
     (letm ((vec (vector-double 3)))
       (setf (data vec) #(-3.21d0 1.0d0 12.8d0))
       (data vec))))
   (lisp-unit:assert-equal
    '("-0.321000000000d+01" "0.100000000000d+01" "0.128000000000d+02")
    (lisp-unit:fp-sequence 
     (letm ((vec (vector-double #(-3.21d0 1.0d0 12.8d0))))
       (data vec))))
   (lisp-unit:assert-equal
    '("0.000000000000d+01" "0.100000000000d+01" "0.000000000000d+01"
      "0.000000000000d+01" "0.000000000000d+01")
    (lisp-unit:fp-sequence 
     (letm ((base (vector-double 5)))
       (set-basis base 1)
       (data base))))
   (lisp-unit:assert-equal
    '("-0.321000000000d+01" "0.100000000000d+01" "0.128000000000d+02")
    (lisp-unit:fp-sequence 
     (letm ((vec1 (vector-double #(-3.21d0 1.0d0 12.8d0)))
	    (vec2 (vector-double 3)))
       (copy vec2 vec1)
       (data vec2))))))
