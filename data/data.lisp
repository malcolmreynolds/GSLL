;********************************************************
; file:        data.lisp                                 
; description: Using GSL storage.                        
; date:        Sun Mar 26 2006 - 16:32                   
; author:      Liam M. Healy                             
; modified:    Fri Apr 14 2006 - 20:14
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Class gsl-data and generic functions
;;;;****************************************************************************

(defclass gsl-data ()
  ((pointer :initarg :pointer :reader pointer)
   (storage-size :initarg :storage-size :reader storage-size )))

(defparameter *print-contents* t)

(defmethod print-object ((object gsl-data) stream)
  (print-data-object object *print-contents* stream))

(defun print-data-object (object contents stream)
  "Print the data object to the stream, possibly showing contents."
  (print-unreadable-object (object stream :type t :identity t)
    (when contents
      (princ (data object) stream))))

;;; Accessing elements
(export 'gsl-aref)
(defgeneric gsl-aref (object &rest indices)
  (:documentation "An element of the data."))

(defgeneric (setf gsl-aref) (value object &rest indices)
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
  (setf (slot-value object 'pointer)
	pointer))

(defmacro defdata (string c-base-type cl-base-type &optional (dimensions 1))
  "For the type named in the string,
   define the allocator (gsl-*-alloc), zero allocator (gsl-*-calloc),
   freeing (gsl-*-free), binary writing (binary-*-write) and
   reading (binary-*-read), formatted writing (write-*-formatted)
   and reading (read-*-formatted) functions."
  (flet ((gsl-name (function-name)
	   (format nil "gsl_~a_~a" string function-name)))
    (let ((indlist
	   (loop for i from 1 to dimensions
		 collect `(,(intern (format nil "N~d" i)) :size)))
	  (object-name (data-object-name string)))
      `(progn
	(defclass ,object-name (gsl-data)
	  ((c-base-type :initform ,c-base-type :reader c-base-type
			:allocation :class)
	   (cl-base-type :initform ,cl-base-type :reader cl-base-type
			 :allocation :class)))
	(defmethod alloc ((object ,object-name))
	  (assign-pointer
	   object
	   (apply
	    (defun-gsl :lambda ,indlist ,(gsl-name "alloc")
		       :c-return-value :return :return (:pointer))
	    (storage-size object))))
	(defmethod calloc ((object ,object-name))
	  (assign-pointer
	   object
	   (apply
	    (defun-gsl :lambda ,indlist ,(gsl-name "calloc")
		       :c-return-value :return :return (:pointer))
	    (storage-size object))))
	(defmethod free ((object ,object-name))
	  (funcall
	   (defun-gsl :lambda ((pointer :pointer)) ,(gsl-name "free")
		      :c-return-value :void)
	   (pointer object)))
	(defun-gsl write-binary ((stream :pointer) ((pointer object) :pointer))
	  ,(gsl-name "fwrite")
	  :method ((object ,object-name) stream))
	(defun-gsl read-binary ((stream :pointer) ((pointer object) :pointer))
	  ,(gsl-name "fread")
	  :method ((object ,object-name) stream))
	(defun-gsl write-formatted
	    ((stream :pointer) ((pointer object) :pointer) (format :string))
	  ,(gsl-name "fprintf")
	  :method ((object ,object-name) stream format))
	(defun-gsl read-formatted
	    ((stream :pointer) ((pointer object) :pointer) (format :string))
	  ,(gsl-name "fscanf")
	  :method ((object ,object-name) stream format))))))

;;;;****************************************************************************
;;;; Making data objects and initializing storage
;;;;****************************************************************************

(export '(make-data with-data))

(defgeneric alloc (object)
  (:documentation "Allocate GSL data; used internally."))

(defgeneric calloc (object)
  (:documentation "Allocate GSL data and clear; used internally."))

(defgeneric free (object)
  (:documentation "Free GSL data; used internally."))

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
;;;; Getting values
;;;;****************************************************************************

(export 'data)
(defgeneric data (object &optional destination)
  (:documentation "Extract the values in the object to a CL object.
   The destination may be a sequence, 'vector, 'list.
   If it is a sequence, that object is filled with the values.
   If it is any other object, a new sequence is made of the type
   specified.")
  ;; Default method is to make a sequence
  (:method ((object gsl-data) &optional (sequence 'vector))
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
	     seq)))

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defgeneric (setf data) (cl-array object)
  (:documentation "Set the values in the object from a CL array.")
  ;; Default method is to read from a sequence
  (:method
   (sequence (object gsl-data))
   (loop for i from 0
	 below (min (length sequence) (apply #'* (storage-size object)))
	 do (setf (gsl-aref object i) (elt sequence i)))))

(export 'set-all)
(defgeneric set-all (object value)
  (:documentation "Set all elements to the value."))

(export 'set-zero)
(defgeneric set-zero (object)
  (:documentation "Set all elements to 0."))

(export 'set-identity)
(defgeneric set-identity (object)
  (:documentation "Set elements to represent the identity."))

(export 'data-valid)
(defgeneric data-valid (object)
  (:documentation "Validate the values in the object."))
