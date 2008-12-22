;; "Data" is bulk arrayed data, like vectors, matrices, permutations,
;; combinations, or histograms.
;; Liam Healy 2008-04-06 21:23:41EDT
;; Time-stamp: <2008-12-21 17:58:34EST data.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; The class gsl-data and element types that make subclasses
;;;;****************************************************************************

(defclass gsl-data (mobject)
  ((cl-array :documentation "The Lisp array.")
   (block-pointer :initform nil :accessor block-pointer
		  :documentation "A pointer to the gsl-block-c.")
   #-native
   (c-pointer :accessor c-pointer :documentation "A pointer to the C array.")
   (dimensions :reader dimensions)
   (total-size :reader total-size)
   (element-type :reader element-type)
   (original-array :reader original-array)
   (offset :reader offset)
   #-native
   (cl-invalid
    :initform t :accessor cl-invalid
    :documentation
    "An indication of whether the Lisp object (slot 'data) agrees with the
     GSL C data.  If NIL, they agree.  If T, they disagree in an unspecified
     way.  If a list of index sets, those indices disagree and the remainder
     are correct.")
   #-native
   (c-invalid
    :initform t :accessor c-invalid
    :documentation
    "An indication of whether the GSL C data agrees with the
     Lisp object (slot 'data).  If NIL, they agree.  If T,
     they disagree in an unspecified
     way.  If a list of index sets, those indices disagree and the remainder
     are correct."))
  (:documentation
   "A superclass for all data storage structures, such as vector, matrix,
   etc."))

(export '(dimensions total-size element-type))

;;; Allowable keys: :dimensions, :initial-contents, :initial-element.
(defmethod initialize-instance :after
    ((object gsl-data) &rest initargs &key &allow-other-keys)
  (with-slots (cl-array dimensions original-array offset total-size) object
    (let ((ffa (apply #'make-ffa (element-type object) initargs)))
      (setf cl-array ffa
	    dimensions (array-dimensions ffa)
	    total-size (array-total-size ffa)))
    #-native (setf (cl-invalid object) nil)
    (multiple-value-bind  (oa index-offset)
	(find-original-array (cl-array object))
      (setf original-array oa
	    offset
	    (* index-offset
	       (cffi:foreign-type-size (cl-cffi (element-type object)))))))
  (alloc-gsl-struct object))

(defmethod print-object ((object gsl-data) stream)
  (print-unreadable-object (object stream :type t) 
    #-native (copy-c-to-cl object)
    (princ (cl-array object) stream)))

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
;;;; Definition of specific data classes
;;;;****************************************************************************

(defparameter *class-element-type* nil
  "The mapping between the class name and the CL element type.")

(defun data-class-name (category element-type)
  "The class name from the type of element."
  ;; e.g. (data-class-name 'vector '(unsigned-byte 8))
  ;; -> VECTOR-UNSIGNED-BYTE-8
  (if (member category '(vector matrix))
      (intern (format nil "~a-~a" category (cl-single element-type))
	      :gsl)
      category))

(defun data-defclass (category superclass)
  "Define all the subclasses based on the known element types."
  (cons 'progn
	(mapcan
	 (lambda (element-type-cl)
	   (let* ((class-name (data-class-name category element-type-cl)))
	     ;; Define the class
	     `((defclass ,class-name
		   (,superclass)
		 ((element-type :initform ',element-type-cl
				:allocation :class)))
	       ;; Push mapping onto *class-element-type*
	       (pushnew ',(cons class-name element-type-cl)
			*class-element-type* :test #'equal))))
	 *array-element-types*)))

;;;;****************************************************************************
;;;; Make data from either the dimensions provided or from the initial values
;;;;****************************************************************************

(export 'make-array*)
(defun make-array*
    (element-type &rest keys &key dimensions initial-contents &allow-other-keys)
  "Make a GSLL array with the given element type,
   :dimensions, :initial-contents and/or :initial-element."
  (apply #'make-instance
	 (data-class-name
	  (if
	   (or
	    (and dimensions (listp dimensions) (eql (length dimensions) 2))
	    (and initial-contents (listp (first initial-contents))))
	   'matrix 'vector)
	  element-type)
	 keys))

(defun hashm-numeric-code (n)
  "Get the appropriate element type for the numeric code n"
  (case n
    ((nil 1) 'double-float)
    (2 '(complex double-float))
    (3 'single-float)
    (4 '(complex single-float))
    (7 '(signed-byte 8))
    (8 '(unsigned-byte 8))
    (15 '(signed-byte 16))
    (16 '(unsigned-byte 16))
    (31 '(signed-byte 32))
    (32 '(unsigned-byte 32))
    (63 '(signed-byte 64))
    (64 '(unsigned-byte 64))))

(set-dispatch-macro-character
 #\# #\m
 (lambda (stream subchar arg)
   (declare (ignore subchar))
   (read-char stream)
   (let ((list (read-delimited-list #\) stream)))
     `(make-array* ',(hashm-numeric-code arg) :initial-contents ',list))))

(defun component-type (eltype)
  (cl-cffi (component-float-type eltype)))

(defun component-size (object)
  (if (subtypep (element-type object) 'complex)
      ;; complex: make array twice as long
      (* 2 (total-size object))
      (total-size object)))

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
  (when (c-invalid object)
    (copy-array-to-pointer
     (cl-array object)
     (c-pointer object)
     (component-type (element-type object))
     (element-type object)
     0
     (component-size object))
    (setf (c-invalid object) nil)))

;;; Called right before maref
#-native
(defun copy-c-to-cl (object)
  "Copy the C array to the CL array."
  (when (cl-invalid object)
    (copy-array-from-pointer
     (cl-array object)
     (c-pointer object)
     (component-type (element-type object))
     (element-type object)
     0
     (component-size object))
    (setf cl-invalid nil)))

#-native
(defun copy-array-to-pointer (array pointer lisp-type index-offset length)
  "Copy length elements from array (starting at index-offset) of type
   lisp-type to the memory area that starts at pointer, coercing the
   elements if necessary."
  (let ((cffi-type (component-type lisp-type)))
    (loop
       for pointer-index :from 0
       :below (if (subtypep lisp-type 'complex) (* 2 length) length)
       :by (if (subtypep lisp-type 'complex) 2 1)
       for array-index :from index-offset
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
    (loop
       for pointer-index :from 0
       :below (if (subtypep lisp-type 'complex) (* 2 length) length)
       :by (if (subtypep lisp-type 'complex) 2 1)
       for array-index :from index-offset
       do
       (setf (row-major-aref array array-index)
	     (complex 
	      (cffi:mem-aref pointer cffi-type pointer-index)
	      (cffi:mem-aref pointer cffi-type (1+ pointer-index)))))))

;;;;****************************************************************************
;;;; C structures
;;;;****************************************************************************

;;; We don't allocate or free the C array data, because that is handled by ffa. 
;;; We can use the GSL functions *_alloc_from_block because they will allocate 
;;; only the structure, but we must use CFFI to allocate the block
;;; structure; otherwise it would try to allocate the C array.
;;; We do not use any of the GSL free functions because they would
;;; free the C array data.

(cffi:defcstruct gsl-block-c
  (size sizet)
  (data :pointer))

(defgeneric alloc-gsl-struct (object)
  (:documentation "Allocate the GSL structure.")
  (:method ((object gsl-data))
    ;; Need to check that all allocations succeeded.
    (unless (block-pointer object)
      (let ((blockptr (cffi:foreign-alloc 'gsl-block-c)))
	(setf (block-pointer object)
	      blockptr
	      (cffi:foreign-slot-value blockptr 'gsl-block-c 'size)
	      (total-size object)
	      (cffi:foreign-slot-value (block-pointer object) 'gsl-block-c 'data)
	      (c-pointer object))
	(let ((array-struct (alloc-from-block object)))
	  (tg:finalize
	   object
	   (lambda ()
	     (unless (eq blockptr array-struct)
	       (cffi:foreign-free blockptr))
	     (cffi:foreign-free array-struct)))
	  (setf (mpointer object) array-struct))))))

;;; For #-native, do the whole thing using _alloc, _free functions.
