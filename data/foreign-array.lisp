;; Foreign arrays (usually in C)
;; Liam Healy 2008-12-28 10:44:22EST foreign-array.lisp
;; Time-stamp: <2009-02-23 15:18:04EST foreign-array.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Object and methods
;;;;****************************************************************************

(export '(dimensions total-size element-type))

#-native
(defgeneric (setf cl-invalid) (value object) ; do nothing 
  (:method (value (object t)) value))

#-native
(defgeneric (setf c-invalid) (value object) ; do nothing 
  (:method (value (object t)) value))

(defclass foreign-array ()
  ((cl-array :initarg :cl-array :documentation "The Lisp array.")
   #-native
   (c-pointer :accessor c-pointer :documentation "A pointer to the C array.")
   (dimensions :reader dimensions)
   (total-size :reader total-size)
   (element-type :initarg :element-type :reader element-type)
   (original-array :reader original-array)
   (offset :reader offset)
   #-native
   (cl-invalid
    :initform t :accessor cl-invalid
    :documentation
    "An indication of whether the Lisp object (slot 'data) agrees with the
     C data.  If NIL, they agree.  If T, they disagree in an unspecified
     way.  If a list of index sets, those indices disagree and the remainder
     are correct.")
   #-native
   (c-invalid
    :initform t :accessor c-invalid
    :documentation
    "An indication of whether the C data agrees with the
     Lisp object (slot 'data).  If NIL, they agree.  If T,
     they disagree in an unspecified
     way.  If a list of index sets, those indices disagree and the remainder
     are correct."))
  (:documentation
   "A superclass for arrays represented in C and CL."))

;;; Allowable keys: :dimensions, :initial-contents, :initial-element.
(defmethod initialize-instance :after
    ((object foreign-array) &rest initargs
     &key dimensions initial-contents initial-element)
  (declare (ignore dimensions initial-contents initial-element))
  (with-slots (cl-array dimensions original-array offset total-size) object
    (unless (and (slot-boundp object 'cl-array) cl-array)
      (setf cl-array (apply #'make-ffa (element-type object) initargs)))
    (setf dimensions (array-dimensions cl-array)
	  total-size (array-total-size cl-array))
    #-native
    (let ((cptr (cffi:foreign-alloc
		 (cl-cffi (element-type object))
		 :count (total-size object))))
      (setf (c-pointer object) cptr)
      (tg:finalize object (lambda () (cffi:foreign-free cptr))))
    #-native (setf (cl-invalid object) nil)
    (multiple-value-bind  (oa index-offset)
	(find-original-array (cl-array object))
      (setf original-array oa
	    offset
	    (* index-offset
	       (cffi:foreign-type-size (cl-cffi (element-type object))))))))

(defparameter *print-contents* t
  "Print the contents of the foreign-array.")

(defmethod print-object ((object foreign-array) stream)
  (print-unreadable-object (object stream :type t) 
    (if *print-contents*
	(progn
	  #-native (copy-c-to-cl object)
	  (princ (cl-array object) stream))
	(format stream "dimensions ~a" (dimensions object)))))

(defun dim0 (object)
  "The first dimension of the object."
  (first (dimensions object)))

(defun dim1 (object)
  "The first dimension of the object."
  (second (dimensions object)))

(defun element-size (object)
  "The size of each element as stored in C."
  (cffi:foreign-type-size (cl-cffi (element-type object))))

;;;;****************************************************************************
;;;; Syncronize C and CL
;;;;****************************************************************************

;;; For implementations with separate C and CL storage (non-native),
;;; the two arrays must match when one side has updated and the other
;;; side wishes to refer to the values.  For native implementations,
;;; these don't do anything because they will always match.

;;; Called in defmfun expansion right before GSL function is entered.
#-native
(defun copy-cl-to-c (object)
  "Copy the CL array to the C array."
  (when (and (typep object 'mobject) (c-invalid object))
    (copy-array-to-pointer
     (slot-value object 'cl-array)
     (c-pointer object)
     (element-type object)
     0
     (total-size object))
    (setf (c-invalid object) nil)))

;;; Called right before maref
#-native
(defun copy-c-to-cl (object)
  "Copy the C array to the CL array."
  (when (and (typep object 'mobject) (cl-invalid object))
    (copy-array-from-pointer
     (slot-value object 'cl-array)
     (c-pointer object)
     (element-type object)
     0
     (total-size object))
    (setf (cl-invalid object) nil)))

#-native
(defun copy-array-to-pointer (array pointer lisp-type index-offset length)
  "Copy length elements from array (starting at index-offset) of type
   lisp-type to the memory area that starts at pointer, coercing the
   elements if necessary."
  (let ((cffi-type (component-type lisp-type)))
    (loop :repeat length
       for array-index :from index-offset
       for pointer-index :from 0 :by (if (subtypep lisp-type 'complex) 2 1)
       do
       (if (subtypep lisp-type 'complex)
	   (setf (cffi:mem-aref pointer cffi-type pointer-index)
		 (realpart (row-major-aref array array-index))
		 (cffi:mem-aref pointer cffi-type (1+ pointer-index))
		 (imagpart (row-major-aref array array-index)))
	   (setf (cffi:mem-aref pointer cffi-type pointer-index)
		 (row-major-aref array array-index))))))

#-native
(defun copy-array-from-pointer (array pointer lisp-type index-offset length)
  "Copy length elements from array (starting at index-offset) of type
   lisp-type from the memory area that starts at pointer, coercing the
   elements if necessary."
  (let ((cffi-type (component-type lisp-type)))
    (loop :repeat length
       for pointer-index :from 0 :by (if (subtypep lisp-type 'complex) 2 1)
       for array-index :from index-offset
       do
       (setf (row-major-aref array array-index)
	     (if (subtypep lisp-type 'complex)
		 (complex 
		  (cffi:mem-aref pointer cffi-type pointer-index)
		  (cffi:mem-aref pointer cffi-type (1+ pointer-index)))
		 (cffi:mem-aref pointer cffi-type pointer-index))))))
