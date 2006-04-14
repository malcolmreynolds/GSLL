;********************************************************
; file:        data.lisp                                 
; description: Using GSL storage.                        
; date:        Sun Mar 26 2006 - 16:32                   
; author:      Liam M. Healy                             
; modified:    Thu Apr 13 2006 - 22:55
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Class gsl-data and generic functions
;;;;****************************************************************************

(defclass gsl-data ()
  ((pointer :initarg :pointer :reader pointer)
   (storage-size :initarg :storage-size :reader storage-size )))

;;; Accessing elements
(export 'gsl-aref)
(defgeneric gsl-aref (object &rest indices)
  (:documentation "An element of the data."))

(defgeneric (setf gsl-aref) (value object &rest indices)
  (:documentation "Set an element of the data."))

;;; Initializing elements
(export 'set-all)
(defgeneric set-all (object value)
  (:documentation "Set all elements to the value."))

(export 'set-zero)
(defgeneric set-zero (object)
  (:documentation "Set all elements to 0."))

(export 'set-identity)
(defgeneric set-identity (object)
  (:documentation "Set elements to represent the identity."))

(export 'data-import)
(defgeneric data-import (object from)
  (:documentation "Import the values from the CL object."))

(export 'data-export)
(defgeneric data-export (object)
  (:documentation "Create a CL object with the values."))

(export 'data-validate)
(defgeneric data-valid (object)
  (:documentation "Validate the values in the object."))

(defgeneric alloc (object)
  (:documentation "Allocate GSL data; used internally."))

(defgeneric calloc (object)
  (:documentation "Allocate GSL data and clear; used internally."))

(defgeneric free (object)
  (:documentation "Free GSL data; used internally."))

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
;;;; Macro gsl-data-functions
;;;;****************************************************************************

(defun data-object-name (string)
  (intern (format nil "GSL-~:@(~a~)" string)))

(defun assign-pointer (object pointer)
  "Check that a GSL data pointer is not null, then assign it to the object."
  (check-null-pointer
   pointer
   :ENOMEM
   (format nil "for ~a." object))
  (setf (slot-value object 'pointer)
	pointer))

(defmacro gsl-data-functions (string base-type &optional (dimensions 1))
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
	  ((base-type :initform ,base-type :reader base-type
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
;;;; Macro with-data
;;;;****************************************************************************

(export 'with-data)
(defmacro with-data ((symbol type size &optional zero) &body body)
  "Allocate GSL data, bind to pointer,
   and then deallocated it when done.  If zero is T, zero the
   contents when allocating."
  (let ((sz (if (listp size) size (list size))))
    `(let ((,symbol
	    (make-instance
	     ',(data-object-name type)
	     :storage-size ,(cons 'list sz))))
      (,(if zero 'calloc 'alloc) ,symbol)
      (unwind-protect 
	   (progn ,@body)
	(free ,symbol)))))

#+new-broken
(defmacro with-data ((symbol type size &optional init validate) &body body)
  "Allocate GSL data, bind to pointer,
   and then deallocated it when done.  If init is T, init the
   contents when allocating."
  (flet ((cl-name (action)
	   (intern (format nil "GSL-~a-~a" type action))))
    (let ((ptr (gensym "PTR")))
      `(let* ((,ptr
	       (,(if (eq init t) (cl-name 'calloc) (cl-name 'alloc))
		 ,@(if (listp size) size (list size))))
	      (,symbol
	       (make-instance ',(intern (format nil "GSL-~a" type))
			      :pointer ,ptr)))
	(check-null-pointer ,ptr
	 :ENOMEM
	 (format nil "For '~a of GSL type ~a." ',symbol ',type))
	(unwind-protect 
	     (progn
	       ,@(case init
		       (:identity `((set-identity ,symbol)))
		       ((t) nil)
		       (t `((data-import ,symbol ,init))))
	       ,@(when
		  validate
		  `((unless (data-valid ,symbol)
		      (error "Invalid ~a, ~a" type init))))
	       ,@body)
	  (,(cl-name 'free) ,ptr))))))

#+new
(defun make-data (type size &optional init validate)
  "Make the GSL data, i.e. vector, matrix, combination, etc."
  (make-instance
   (intern (format nil "GSL-~a" type))
   :pointer pointer)
  (case init
    ((:fast :identity) (cl-name 'alloc))
    ((t) nil)
    (t `((data-import ,symbol ,init))))
  (case init
    (:identity `((set-identity ,symbol)))))

#+new
(defun free-data (data)
  )


;;;(with-data (p permutation 5 #(2 3 4 0) t)  foo)

#|
;;; Another version, that takes a passed-in array
(defmacro with-data*
    ((symbol type size &optional init validate) &body body)
  (let ((ptr (gensym "PTR")))
    `(cffi::with-foreign-array (,ptr ,type ,size)
      (let* ((,symbol
	      (make-instance ',(intern (format nil "GSL-~a" type))
			     :pointer ,ptr)))
	(progn
	  ,@(case init
		  (:identity `((set-identity ,symbol)))
		  ((t) nil)
		  (t `((data-import ,symbol ,init))))
	  ,@(when
	     validate
	     `((unless (data-valid ,symbol)
		 (error "Invalid ~a, ~a" type init))))
	  ,@body)))))

;;; test
#+test
(with-data* (comb combination (4 i) t)
  (loop collect (combination-list comb)
	while (combination-next comb)))


#+expansion
(CFFI::WITH-FOREIGN-ARRAY
    ;;;(#:PTR3308 COMBINATION (4 I))	; wrong
    (#:PTR3308 :size ????)	; want
  ;;; need to C-structure here.
  (LET* ((COMB
	  (MAKE-INSTANCE 'GSL-COMBINATION
			 :POINTER
			 #:PTR3308)))
    (PROGN
      (LOOP COLLECT
	    (COMBINATION-LIST COMB)
	    WHILE
	    (COMBINATION-NEXT COMB)))))
|#
