;********************************************************
; file:        data.lisp                                 
; description: Using GSL storage.                        
; date:        Sun Mar 26 2006 - 16:32                   
; author:      Liam M. Healy                             
; modified:    Tue May 30 2006 - 17:38
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; To do:
;;; - create a CL object of class gsl-data with the right GSL
;;; pointer just from a raw CL object
;;; - recreate GSL object should be possible to recreate the C object
;;; (need this?)
;;; - master list of objects, for manual memory management?

;;;;****************************************************************************
;;;; Class gsl-data and generic functions
;;;;****************************************************************************

(defclass gsl-data ()
  ((pointer :initarg :pointer :accessor pointer
	    :documentation "A C pointer to the GSL representation of the data.")
   (storage-size :initarg :storage-size :reader storage-size)
   (data :accessor data-cache
	 :documentation "The Lisp object corresponding to the GSL data.")
   (cl-invalid
    :initform t :accessor cl-invalid
    :documentation
    "An indication of whether the Lisp object (slot 'data) agrees with the
     GSL C data.  If NIL, they agree.  If T, they disagree in an unspecified
     way.  If a list of index sets, those indices disagree and the remainder
     are correct."))
  (:documentation
   "A superclass for all GSL data storage structures, such as vector, matrix,
   etc."))

(defmethod print-object ((object gsl-data) stream)
  (print-data-object object *print-array* stream))

(defun print-data-object (object contents stream)
  "Print the data object to the stream, possibly showing contents."
  (print-unreadable-object (object stream :type t :identity t)
    (when contents
      (princ (data object) stream))))

(defgeneric gsl-array (object)
  (:documentation "A pointer to the GSL array with the data contents."))

(defun dim0 (object)
  "The first dimension of the object."
  (first (storage-size object)))

(defun dim1 (object)
  "The second dimension of the object."
  (second (storage-size object)))

(defgeneric cl-base-type (object)
  (:documentation "The CL type of an element."))

;;; Accessing elements
(export 'gsl-aref)
(defgeneric gsl-aref (object &rest indices)
  (:documentation "An element of the data."))

(defgeneric (setf gsl-aref) (value object &rest indices)
  (:method :after (value (object gsl-data) &rest indices)
    (push indices (cl-invalid object)))
  (:documentation "Set an element of the data."))

(export '(write-binary read-binary write-formatted read-formatted))
(defgeneric write-binary (object stream)
  (:documentation "Write the binary GSL data."))

(defgeneric read-binary (object stream)
  (:documentation "Read the binary GSL data."))

(defgeneric write-formatted (object stream format)
  (:documentation "Write the formatted GSL data."))

(defgeneric read-formatted (object stream format)
  (:documentation "Read the formatted GSL data."))

;;;;****************************************************************************
;;;; Macro defdata to define class etc.
;;;;****************************************************************************

(defun data-object-name (string)
  (intern (format nil "GSL-~:@(~a~)" string)))

(defun assign-pointer (object pointer)
  "Check that a GSL data pointer is not null, then assign it to the object."
  (check-null-pointer
   pointer
   :ENOMEM
   (format nil "for ~a."
	   (with-output-to-string (stream)
	     (print-data-object object nil stream))))
  (setf (pointer object) pointer))

(defmacro defdata (string cl-base-type &optional (dimensions 1))
  "For the type named in the string,
   define the allocator (gsl-*-alloc), zero allocator (gsl-*-calloc),
   freeing (gsl-*-free), binary writing (binary-*-write) and
   reading (binary-*-read), formatted writing (write-*-formatted)
   and reading (read-*-formatted) functions."
  (flet ((gsl-name (function-name)
	   (format nil "gsl_~a_~a" string function-name)))
    (let ((cargs
	   (cons '((pointer object) :pointer)
		 (loop for i below dimensions collect `((nth ,i (storage-size object)) :size))))
	  (object-name (data-object-name string)))
      `(progn
	(defclass ,object-name (gsl-data)
	  ((cl-base-type :initform ,cl-base-type :reader cl-base-type
			 :allocation :class)))
	(defun-gsl alloc ((object ,object-name))
	  ,(gsl-name "alloc") ,cargs
	  :type :method
	  :c-return (cr :pointer)
	  :return ((assign-pointer object cr)))
	(defun-gsl calloc ((object ,object-name))
	  ,(gsl-name "calloc") ,cargs
	  :type :method
	  :c-return (cr :pointer)
	  :return ((assign-pointer object cr)))
	(defun-gsl free ((object ,object-name))
	  ,(gsl-name "free") (((pointer object) :pointer))
	  :type :method
	  :c-return :void)
	(defun-gsl write-binary ((object ,object-name) stream)
	  ,(gsl-name "fwrite") ((stream :pointer) ((pointer object) :pointer))
	  :type :method)
	(defun-gsl read-binary ((object ,object-name) stream)
	  ,(gsl-name "fread") ((stream :pointer) ((pointer object) :pointer))
	  :type :method)
	(defun-gsl write-formatted ((object ,object-name) stream format)
	  ,(gsl-name "fprintf")
	  ((stream :pointer) ((pointer object) :pointer) (format :string))
	  :type :method)
	(defun-gsl read-formatted ((object ,object-name) stream format)
	  ,(gsl-name "fscanf")
	  ((stream :pointer) ((pointer object) :pointer) (format :string))
	  :type :method)))))

;;;;****************************************************************************
;;;; Making data objects and initializing storage
;;;;****************************************************************************

(export '(make-data with-data))

(defgeneric alloc (object)
  (:documentation "Allocate GSL data; used internally."))

(defgeneric calloc (object)
  (:documentation "Allocate GSL data and clear; used internally."))

(export 'free)
(defgeneric free (object)
  (:documentation "Free GSL data.")
  (:method :after ((object gsl-data)) (setf (pointer object) nil)))

(defun make-data (type zero &rest size)
  "Make the GSL data object, including the allocation of space.
   The user is responsible for calling #'free to free the foreign
   memory when done."
  (let ((obj
	 (make-instance
	  (data-object-name type)
	  :storage-size size)))
    (if zero (calloc obj) (alloc obj))
    obj))

(defmacro with-data ((symbol type size &optional zero) &body body)
  "Allocate GSL data, bind to pointer,
   and then deallocated it when done.  If zero is T, zero the
   contents when allocating."
  `(let ((,symbol
	  (make-data ',type  ,zero ,@(if (listp size) size (list size)))))
    (unwind-protect 
	 (progn ,@body)
      (free ,symbol))))

;;;;****************************************************************************
;;;; Getting values into CL
;;;;****************************************************************************

(defun cl-invalidate (&rest objects)
  "Mark the CL image of the GSL array as invalid."
  (mapc (lambda (obj) (setf (cl-invalid obj) t))
	objects))

(export 'data)
(defgeneric data (object &optional destination)
  (:documentation "Extract the values in the object to a CL object.
   The destination may be a sequence, 'vector, 'list.
   If it is a sequence, that object is filled with the values.
   If it is any other object, a new sequence is made of the type
   specified.")
  ;; Default method is to make a sequence
  (:method ((object gsl-data) &optional (sequence 'vector))
    (if (eq (cl-invalid object) t)
	;; set everything
	(let* ((total-size (apply #'* (storage-size object)))
	       (seq
		(case sequence
		  (list (make-list total-size))
		  ((nil vector)
		   (make-array (list total-size)
			       :element-type (cl-base-type object)))
		  (t sequence))))
	  (loop for i from 0
	     below (min (length seq) total-size)
	     do (setf (elt seq i) (gsl-aref object i)))
	  seq)
	;; set selected
	(let ((seq (data-cache object)))
	  (mapc (lambda (is)
		  (let ((i (first is)))
		    (setf (elt seq i) (gsl-aref object i))))
		(cl-invalid object))
	  seq)))
  ;; Around method looks for cached value and returns it if valid;
  ;; otherwise computes CL element(s).
  (:method :around ((object gsl-data) &optional (sequence 'vector))
	   (declare (ignore sequence))
	   (when (cl-invalid object)
	     (setf (data-cache object)
		   (call-next-method)
		   (cl-invalid object)
		   nil))
	   (data-cache object)))

;;;;****************************************************************************
;;;; Setting values from CL
;;;;****************************************************************************

(defgeneric (setf data) (cl-array object)
  (:documentation "Set the values in the object from a CL array.")
  ;; Default method is to read from a sequence
  (:method (sequence (object gsl-data))
    (loop for i from 0
       below (min (length sequence) (apply #'* (storage-size object)))
       do (setf (gsl-aref object i) (elt sequence i))))
  (:method :after (source (object gsl-data))
	   (setf (cl-invalid object) t)))

(export 'set-all)
(defgeneric set-all (object value)
  (:documentation "Set all elements to the value.")
  (:method :after ((object gsl-data) value) (cl-invalidate object)))

(export 'set-zero)
(defgeneric set-zero (object)
  (:documentation "Set all elements to 0.")
  (:method :after ((object gsl-data)) (cl-invalidate object)))

(export 'set-identity)
(defgeneric set-identity (object)
  (:documentation "Set elements to represent the identity.")
  (:method :after ((object gsl-data)) (cl-invalidate object)))

(export 'data-valid)
(defgeneric data-valid (object)
  (:documentation "Validate the values in the object."))
