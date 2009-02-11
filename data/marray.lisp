;; A "marray" is an array in both GSL and CL
;; Liam Healy 2008-04-06 21:23:41EDT
;; Time-stamp: <2009-02-10 22:39:58EST marray.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; The class marray and its construction
;;;;****************************************************************************

(defclass marray (mobject foreign-array)
  ()
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
	    (cffi:foreign-slot-value blockptr 'gsl-block-c 'data)
	    (c-pointer object))
      (let ((array-struct (alloc-from-block object blockptr)))
	(setf (slot-value object 'mpointer) array-struct)
	(tg:finalize object
		     (lambda ()
		       (cffi:foreign-free blockptr)
		       (cffi:foreign-free array-struct)))))))

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
    (element-type &rest keys &key dimensions initial-contents from-pointer cl-array
     &allow-other-keys)
  "Make a GSLL array with the given element type,
   :dimensions, and :initial-contents, :initial-element or :cl-array.
   If the :cl-array is supplied, it should be a CL array generated with #'make-ffa.
   If a pointer to a GSL object is given in :from-pointer, create
   an object with duplicate contents; if a matrix, :dimensions must be set to 2."
  ;; Some functions in solve-minimize-fit return a pointer to a GSL
  ;; vector of double-floats.  With the :from-pointer argument, this
  ;; function turn that into a foreign-friendly array.  There is no
  ;; choice but to copy over the data even on native implementations;
  ;; because GSL is doing the mallocing, the data are not
  ;; CL-accessible.
  (if from-pointer
      (make-marray element-type
		   :initial-contents
		   (contents-from-pointer
		    from-pointer
		    (if (eql dimensions 2) 'gsl-matrix-c 'gsl-vector-c)
		    element-type))
      (apply #'make-instance
	     (data-class-name
	      (if
	       (or
		(and dimensions (listp dimensions) (eql (length dimensions) 2))
		(and initial-contents (listp (first initial-contents)))
		(and cl-array (eql (length (array-dimensions cl-array)) 2)))
	       'matrix 'vector)
	      element-type)
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
;;;; C structures
;;;;****************************************************************************

;;; For #-native, do the whole thing using _alloc, _free functions.
