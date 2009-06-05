;; A "marray" is an array in both GSL and CL
;; Liam Healy 2008-04-06 21:23:41EDT
;; Time-stamp: <2009-06-05 08:45:44EDT marray.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; The class marray and its construction
;;;;****************************************************************************

(defclass marray (mobject foreign-array)
  ((block-pointer :initarg :block-pointer :reader block-pointer)
   (total-size :reader size))
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
	    (size object)
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
		   (+ (offset object) (size object)))
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
			*class-element-type* :test #'equal)
	       (export ',class-name))))
	 *array-element-types*)))

;;;;****************************************************************************
;;;; Make data from either the dimensions provided or from the initial values
;;;;****************************************************************************

(export 'make-marray)
(defun make-marray
    (class-or-element-type &rest keys &key dimensions initial-contents cl-array
     &allow-other-keys)
  "Make a GSLL array with the given element type,
   :dimensions, and :initial-contents, :initial-element or :cl-array.
   If the :cl-array is supplied, it should be a CL array generated
   with #'make-ffa."
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
	 keys))

;;; The reader macro #m will read a list of arguments, evaluating the
;;; contents, and construct a marray from it.  If the symbol ^ occurs
;;; in the list, it indicates the object being made is a matrix, and
;;; this symbol indicates the end of each row.  For compatibility
;;; reasons, a list of numeric values may be given for each row also.

;;; Examples:
;;; #m(1 2 (exp 1))
;;; #<VECTOR-DOUBLE-FLOAT #(1.0d0 2.0d0 2.7182817459106445d0)>
;;; #m(1 2 3 ^ pi (cos (/ pi 4)) -12)
;;; #<MATRIX-DOUBLE-FLOAT #2A((1.0d0 2.0d0 3.0d0)
;;;                       (3.141592653589793d0 0.7071067811865476d0 -12.0d0))>
;;; #2m(2 1 3 -1)
;;; #<VECTOR-COMPLEX-DOUBLE-FLOAT #(#C(2.0d0 1.0d0) #C(3.0d0 -1.0d0))>

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

(defun quote-numeric-list (s)
  (if (and (listp s) (numberp (first s)))
      `',s
      s))

(defvar *row-separator* '^)
(export *row-separator*)

(defun conslist (l) (cons 'list l))

(set-dispatch-macro-character
 #\# #\m
 (lambda (stream subchar arg)
   (declare (ignore subchar))
   (read-char stream)
   (let ((list (read-delimited-list #\) stream)))
     `(make-marray
       ',(hashm-numeric-code arg)
       :initial-contents
       ,(if (find *row-separator* list)
	    (conslist
	     (mapcar 'conslist
		     (cl-utilities:split-sequence *row-separator* list)))
	    `(list ,@(mapcar 'quote-numeric-list list)))))))

;;;;****************************************************************************
;;;; Copy to and from bare mpointers 
;;;;****************************************************************************

(defgeneric contents-from-pointer (pointer struct-type &optional element-type)
  (:documentation
   "Create a contents list from the GSL object of type struct-type
    referenced by pointer."))

(defmethod copy-making-destination ((pointer #.+foreign-pointer-class+))
  (foreign-pointer-method
   pointer
   ;; Default assumption when destination isn't given in #'copy is
   ;; that this should make a vector-double-float.
   (copy-to-destination pointer 'vector-double-float)))

(defmethod copy-to-destination
    ((pointer #.+foreign-pointer-class+) (class-name symbol))
  (foreign-pointer-method
   pointer
   (make-marray
    class-name
    :initial-contents
    (contents-from-pointer
     pointer
     (if (subtypep class-name 'matrix) 'gsl-matrix-c 'gsl-vector-c)
     (lookup-type class-name *class-element-type*)))))

;; Some functions in solve-minimize-fit return a pointer to a GSL
;; vector of double-floats.  This function turns that into a
;; foreign-friendly array.  There is no choice but to copy over the
;; data even on native implementations; because GSL is doing the
;; mallocing, the data are not CL-accessible.
