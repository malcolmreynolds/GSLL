;********************************************************
; file:        data.lisp                                 
; description: Using GSL storage.                        
; date:        Sun Mar 26 2006 - 16:32                   
; author:      Liam M. Healy                             
; modified:    Tue Apr  4 2006 - 22:58
;********************************************************
;;; $Id: $

(in-package :gsl)

(defmacro gsl-data-functions (string &optional (dimensions 1))
  "For the type named in the string,
   define the allocator (gsl-*-alloc), zero allocator (gsl-*-calloc),
   freeing (gsl-*-free), binary writing (binary-*-write) and
   reading (binary-*-read), formatted writing (write-*-formatted)
   and reading (read-*-formatted) functions."
  (flet ((gsl-name (function-name)
	   (format nil "gsl_~a_~a" string function-name))
	 (cl-name (action kind)
	   (intern (format nil "~a-~:@(~a~)-~a" action string kind))))
    (let ((indlist
	   (loop for i from 1 to dimensions
		 collect `(,(intern (format nil "N~d" i)) :size))))
    `(progn
       (cffi:defcfun ,(gsl-name "alloc") :pointer ,@indlist)
       (cffi:defcfun ,(gsl-name "calloc") :pointer ,@indlist)
       (cffi:defcfun ,(gsl-name "free") :void (pointer :pointer))
       (defun ,(cl-name 'write 'binary) (object stream)
	 (check-gsl-status
	  (cffi:foreign-funcall
	   ,(gsl-name "fwrite")
	   :pointer stream
	   :pointer object
	   :int)
	  ',(cl-name 'write 'binary)))
       (defun ,(cl-name 'read 'binary) (object stream)
	 (check-gsl-status
	  (cffi:foreign-funcall
	   ,(gsl-name "fread")
	   :pointer stream
	   :pointer object
	   :int)
	  ',(cl-name 'read 'binary)))
       (defun ,(cl-name 'write 'formatted) (object stream format)
	 "Format the block data to a stream; format is a string,
   one of %g, %e, %f."
	 (check-gsl-status
	  (cffi:foreign-funcall
	   "gsl_block_fprintf"
	   :pointer stream
	   :pointer object
	   :string format
	   :int)
	  ',(cl-name 'write 'formatted)))
       (defun ,(cl-name 'read 'formatted) (object stream)
	 "Read the formatted block data from a stream."
	 (check-gsl-status
	  (cffi:foreign-funcall
	   "gsl_block_fscanf"
	   :pointer stream
	   :pointer object
	   :int)
	  ',(cl-name 'read 'formatted)))))))

(defclass gsl-data ()
  ((pointer :initarg :pointer :reader pointer)))

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


(export 'with-data)
(defmacro with-data ((symbol type size &optional zero) &body body)
  "Allocate GSL data, bind to pointer,
   and then deallocated it when done.  If zero is T, zero the
   contents when allocating."
  (flet ((cl-name (action)
	   (intern (format nil "GSL-~a-~a" type action))))
    (let ((ptr (gensym "PTR")))
      `(let* ((,ptr
	       (,(if zero (cl-name 'alloc) (cl-name 'alloc))
		 ,@(if (listp size) size (list size))))
	      (,symbol
	       (make-instance ',(intern (format nil "GSL-~a" type))
			      :pointer ,ptr)))
	(check-null-pointer ,ptr
	 :ENOMEM
	 (format nil "For '~a of GSL type ~a." ',symbol ',type))
	#+old
	(when (cffi:null-pointer-p ,ptr)
	  (error 'gsl-error
		 :gsl-errno (cffi:foreign-enum-value 'gsl-errorno :ENOMEM)
		 :gsl-reason
		 (format nil "For '~a of GSL type ~a." ',symbol ',type)))
	(unwind-protect 
	     (progn ,@body)
	  (,(cl-name 'free) ,ptr))))))
