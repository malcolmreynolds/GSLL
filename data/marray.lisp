;; A "marray" is an array in both GSL and CL
;; Liam Healy 2008-04-06 21:23:41EDT
;; Time-stamp: <2009-03-20 11:33:31EDT marray.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; The class marray and its construction
;;;;****************************************************************************

(defclass marray (mobject foreign-array)
  ((block-pointer :initarg :block-pointer :reader block-pointer))
  (:documentation
   "A superclass for arrays represented in GSL and CL."))

(cffi:defcstruct gsl-block-c		; The GSL struct
  (size sizet)
  (data :pointer))

;;; We don't allocate or free the C array data, because that is
;;; handled by foreign-array.  We can use the GSL functions
;;; *_alloc_from_block because they will allocate only the structure,
;;; but we must use CFFI to allocate the block structure; otherwise
;;; GSL would try to allocate the C array.  We do not use any of the
;;; GSL free functions because they would free the C array data.

(defmethod initialize-instance :after ((object marray) &rest initargs)
  (declare (ignore initargs)) ; required by &rest arg for foreign-array?
  ;; Need to check that all allocations succeeded.
  ;; Don't do anything if mpointer has been assigned (shouldn't happen)
  ;; or this is a permutation or combination (they have their own methods).
  (unless (and (slot-boundp object 'mpointer) (mpointer object))
    (let ((blockptr (cffi:foreign-alloc 'gsl-block-c)))
      (setf (cffi:foreign-slot-value blockptr 'gsl-block-c 'size)
	    (total-size object)
	    (slot-value object 'block-pointer)
	    blockptr)
      (let ((array-struct (alloc-from-block object blockptr)))
	(setf (slot-value object 'mpointer) array-struct)
	#-native (set-struct-array-pointer object)
	(tg:finalize object
		     (lambda ()
		       (cffi:foreign-free blockptr)
		       (cffi:foreign-free array-struct)))))))

(defun set-struct-array-pointer (object)
  "Set the pointer in the 'data slot of the foreign structs to be the
   current c-pointer value.  In non-native implementations this need
   be called only once when the marray is made.  In native implementations
   it is called whenever mpointer is requested because of the possibility
   that a GC moved the pointer."
  (setf (cffi:foreign-slot-value (block-pointer object) 'gsl-block-c 'data)
	(c-pointer object)
	;; alloc-from-block automatically copies over the data pointer
	;; from the block to the vector/matrix; we must do that manually here
	(cffi:foreign-slot-value
	 (slot-value object 'mpointer)
	 (if (typep object 'matrix) 'gsl-matrix-c 'gsl-vector-c)
	 'data)
	(c-pointer object)))

#+native
(defmethod mpointer ((object marray))
  "Compute the c-pointer of the array and place it in the GSL struct
   because the stored version is untrustworthy unless it was computed
   within the same native-pointer-protect form as this mpointer
   extraction."
  (set-struct-array-pointer object)
  (call-next-method))

(defmethod make-load-form ((object marray) &optional env)
  (declare (ignore env))
  `(make-marray
    ',(element-type object)
    :initial-contents
    ',(loop for elt across
	   (subseq (original-array object)
		   (offset object)
		   (+ (offset object) (total-size object)))
	   collect elt)))

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

(export 'make-marray)
(defun make-marray
    (class-or-element-type &rest keys &key dimensions initial-contents from-pointer cl-array
     &allow-other-keys)
  "Make a GSLL array with the given element type,
   :dimensions, and :initial-contents, :initial-element or :cl-array.
   If the :cl-array is supplied, it should be a CL array generated with #'make-ffa.
   If a pointer to a GSL object is given in :from-pointer, create
   an object with duplicate contents; if a matrix, :dimensions must be a list
   of length 2 (contents are unimportant)."
  ;; Some functions in solve-minimize-fit return a pointer to a GSL
  ;; vector of double-floats.  With the :from-pointer argument, this
  ;; function turn that into a foreign-friendly array.  There is no
  ;; choice but to copy over the data even on native implementations;
  ;; because GSL is doing the mallocing, the data are not
  ;; CL-accessible.
  (if from-pointer
      (make-marray class-or-element-type
		   :initial-contents
		   (contents-from-pointer
		    from-pointer
		    (if (and (listp dimensions)
			     (eql (length dimensions) 2))
			'gsl-matrix-c 'gsl-vector-c)
		    (if (subtypep class-or-element-type 'marray)
			(lookup-type class-or-element-type *class-element-type*)
			class-or-element-type)))
      (apply #'make-instance
	     (if (subtypep class-or-element-type 'marray)
		 class-or-element-type
		 (data-class-name
		  (if
		   (or
		    (and dimensions (listp dimensions) (eql (length dimensions) 2))
		    (and initial-contents (listp (first initial-contents)))
		    (and cl-array (eql (length (array-dimensions cl-array)) 2)))
		   'matrix 'vector)
		  class-or-element-type))
	     keys)))

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
     `(make-marray ',(hashm-numeric-code arg) :initial-contents ',list))))

(defun component-type (eltype)
  (cl-cffi (component-float-type eltype)))

(defun component-size (object)
  (if (subtypep (element-type object) 'complex)
      ;; complex: make array twice as long
      (* 2 (total-size object))
      (total-size object)))

;;;;****************************************************************************
;;;; Copy to and from bare mpointers 
;;;;****************************************************************************

(defgeneric contents-from-pointer (pointer struct-type &optional element-type)
  (:documentation
   "Create a contents list from the GSL object of type struct-type
    referenced by pointer."))

(defmethod copy-making-destination ((pointer #.+foreign-pointer-class+))
  (if (typep pointer +foreign-pointer-type+)
      ;; Default assumption when destination isn't given in #'copy is
      ;; that this should make a vector-double-float.
      (copy-to-destination pointer 'vector-double-float)
      (call-next-method)))

(defmethod copy-to-destination ((pointer #.+foreign-pointer-class+) (class-name symbol))
  (if (typep pointer +foreign-pointer-type+)
      (make-marray class-name :from-pointer pointer)
      (call-next-method)))
