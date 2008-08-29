;; Data using ffa
;; Liam Healy 2008-04-06 21:23:41EDT data-ffa.lisp
;; Time-stamp: <2008-08-28 21:31:49EDT data.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; The class gsl-data and element types that make subclasses
;;;;****************************************************************************

(defclass gsl-data ()
  ((cl-array :initarg :cl-array :documentation "The Lisp array.")
   (mpointer :initarg :mpointer :accessor mpointer
	     :documentation "A pointer to the GSL representation of the data.")
   (block-pointer :initform nil :accessor block-pointer
		  :documentation "A pointer to the gsl-block-c.")
   (c-pointer :initarg :c-pointer :accessor c-pointer
	      :documentation "A pointer to the C array.")
   (dimensions :initarg :dimensions :reader dimensions)
   (total-size :initarg :total-size :reader total-size)
   (element-type :initarg :element-type :reader element-type)
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

(defmethod print-object ((object gsl-data) stream)
  (print-unreadable-object (object stream :type t) 
    (copy-c-to-cl object)
    (princ (cl-array object) stream)))

(defun dim0 (object)
  "The first dimension of the object."
  (first (dimensions object)))

(defun dim1 (object)
  "The first dimension of the object."
  (second (dimensions object)))

(defun element-size (object)
  "The size of each element as stored in C."
  (cffi:foreign-type-size (cl-ffa (element-type object))))

;;;;****************************************************************************
;;;; Definition of specific data classes
;;;;****************************************************************************

(defparameter *class-element-type* nil
  ;;; This is needed for make-data-from-dimensions
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
	       ;; Define the letm-expansion method
	       (defmethod letm-expansion
		   (symbol (type (eql ',class-name)) args body)
		 (expand-data symbol type (first args) (second args) body))
	       ;; Push mapping onto *class-element-type*
	       (pushnew ',(cons class-name element-type-cl)
		*class-element-type* :test #'equal)
	       ;; Set up dummy function to provide arglist and documentation
	       (arglist-only
		,class-name
		,(format nil "A ~(~a~) of element type ~(~a~)."
			 category element-type-cl)
		dimensions-or-init
		sync-array-on-exit))))
	 *array-element-types*)))

;;;;****************************************************************************
;;;; Make data from either the dimensions provided or from the initial values
;;;;****************************************************************************

(defun make-data-from-array (class array)
  "Make the data object from the CL array make with make-array*."
  (make-instance class
		 :cl-array array
		 :mpointer nil	; this will be set by :before method below.
		 :c-pointer nil		; this will be set by defmfun
		 :dimensions (array-dimensions array)
		 :total-size (array-total-size array)))

(defun make-data-from-dimensions (type dimensions)
  (make-data-from-array
   type
   (make-array* dimensions (lookup-type type *class-element-type*))))

(defun make-data (type array-or-dimensions)
  (if (eq type 'combination)
      (make-data-combination array-or-dimensions)
      (if (typep array-or-dimensions 'array)
	  (make-data-from-array type array-or-dimensions)
	  (make-data-from-dimensions type array-or-dimensions))))

;;;;****************************************************************************
;;;; Expand letm bindings
;;;;****************************************************************************

(defun component-type (eltype)
  (cl-ffa (component-float-type eltype)))

(defun component-size (object)
  (if (subtypep (element-type object) 'complex)
      ;; complex: make array twice as long
      (* 2 (total-size object))
      (total-size object)))

(defun numtypespec (sexp)
  "The sexp is a type specification for a numerical type."
  (ignore-errors (subtypep sexp 'number)))

(defun abody (&rest type-contents)
  "Make an array of a specific element type from the literal contents supplied.
   If no type is given, it is assumed to be 'double-float."
  (let* ((firsttype (numtypespec (first type-contents)))
	 (element-type
	  (if firsttype
	      (first type-contents)
	      'double-float))
	 (cont
	  (if firsttype
	      (rest type-contents)
	      type-contents)))
    (make-array*
     (if (listp (first cont))		; make a matrix
	 (list (length cont) (length (first cont)))
	 (list (length cont)))		; make a vector
     element-type
     :initial-contents
     (if (listp (first cont))
	 (apply #'append cont)		; flatten lists
	 cont))))

(export 'a)
(defmacro a (&rest type-contents)
  "Create an array (vector or matrix) from the literal contents."
  `(abody ,@(mapcar (lambda (e) `(quote ,e)) type-contents)))

(defun expand-data (symbol type init-or-spec sync-exit body)
  "Expand the form for a gsl-data object.  The symbol is bound
   within the body to the object.  The argument
   init-or-spec is either the
   array dimensions or something made by make-array*."
  (cl-utilities:with-unique-names (cptr eltype)
    `(macrolet
      ;; #'els is a convenience macro to define the make-array*
      ((a (&rest contents)
	`(abody ',',(lookup-type type *class-element-type*)
	  ,@(mapcar (lambda (e) `(quote ,e)) contents)))
       (a* (&rest contents)
	`(abody ',',(lookup-type type *class-element-type*)
	  ,@contents)))
      (let* ((,symbol (make-data ',type ,init-or-spec))
	     (,eltype (element-type ,symbol)))
	(ffa:with-pointer-to-array
	    ((cl-array ,symbol)
	     ,cptr
	     (component-type ,eltype)
	     (component-size ,symbol)
	     nil)		      ; we need to allow nil direction
	  (unwind-protect
	       (multiple-value-prog1
		   (progn
		     (setf (c-pointer ,symbol) ,cptr)
		     ,@body)
		 ,@(when sync-exit `((copy-c-to-cl ,symbol))))
	    (free-gsl-struct ,symbol)))))))

;;;;****************************************************************************
;;;; Syncronize C and CL
;;;;****************************************************************************

;;; For implementations with separate C and CL storage (non-native),
;;; the two arrays must match when one side has updated and the other
;;; side wishes to refer to the values.  For native implementations,
;;; these don't do anything because they will always match.

;;; Called in defmfun expansion right before GSL function is entered.
(defun copy-cl-to-c (object)
  "Copy the CL array to the C array."
  (declare (ignorable object))
  #-native
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
(defun copy-c-to-cl (object)
  "Copy the C array to the CL array."
  (declare (ignorable object))
  #-native
  (when (cl-invalid object)
    (copy-array-from-pointer
     (cl-array object)
     (c-pointer object)
     (component-type (element-type object))
     (element-type object)
     0
     (component-size object))
    (setf cl-invalid nil)))

;;; My replacement for ffa's routines.  I don't do coercions or checks
;;; on type as they are unnecessary, and I handle complex types.

#-native
(defun copy-array-to-pointer (array pointer lisp-type index-offset length)
  "Copy length elements from array (starting at index-offset) of type
   lisp-type to the memory area that starts at pointer, coercing the
   elements if necessary."
  (let ((cffi-type (component-type lisp-type)))
    (iter:iter
      (iter:for pointer-index :from 0
		:below (if (subtypep lisp-type 'complex) (* 2 length) length)
		:by (if (subtypep lisp-type 'complex) 2 1))
      (iter:for array-index :from index-offset)
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
    (iter:iter
      (iter:for pointer-index :from 0
		:below (if (subtypep lisp-type 'complex) (* 2 length) length)
		:by (if (subtypep lisp-type 'complex) 2 1))
      (iter:for array-index :from index-offset)
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

(defun alloc-gsl-struct (object)
  ;; Allocate the GSL structure; this should only be called from #'mpointer
  ;; thus it will be created only when first needed.
  (unless (c-pointer object) (error "No C array.")) ; safety while developing
  (let ((blockptr (cffi:foreign-alloc 'gsl-block-c)))
    (setf (block-pointer object)
	  blockptr
	  (cffi:foreign-slot-value blockptr 'gsl-block-c 'data)
	  (c-pointer object)
	  (cffi:foreign-slot-value blockptr 'gsl-block-c 'size)
	  (total-size object)
	  (mpointer object)
	  (alloc-from-block object))))

(defmethod mpointer :before ((object gsl-data))
  "Make a GSL struct if there isn't one already."
  (unless (slot-value object 'mpointer)
    (alloc-gsl-struct object)
    nil))

(defun free-gsl-struct (object)
  "Free the C structure(s) and memory for data."
  ;; This should happen immediately after the with-pointer-to-array
  ;; and substitutes for the GSL "_free" functions.
  (when (block-pointer object)
    (unless (eq (block-pointer object) (mpointer object))
      (cffi:foreign-free (block-pointer object))) ; free the block struct
    (setf (block-pointer object) nil))
  (when (mpointer object)
    (cffi:foreign-free (mpointer object)) ; free the larger struct
    (setf (mpointer object) nil)))
