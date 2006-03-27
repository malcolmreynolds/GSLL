;********************************************************
; file:        data.lisp                                 
; description: Using GSL storage.                        
; date:        Sun Mar 26 2006 - 16:32                   
; author:      Liam M. Healy                             
; modified:    Sun Mar 26 2006 - 18:17
;********************************************************
;;; $Id: $

(in-package :gsl)

(defmacro gsl-data-functions (string)
  "For the type named in the string,
   define the allocator (gsl-*-alloc), zero allocator (gsl-*-calloc),
   freeing (gsl-*-free), binary writing (binary-*-write) and
   reading (binary-*-read), formatted writing (write-*-formatted)
   and reading (read-*-formatted) functions."
  (flet ((gsl-name (function-name)
	   (format nil "gsl_~a_~a" string function-name))
	 (cl-name (action kind)
	   (intern (format nil "~a-~:@(~a~)-~a" action string kind))))
    `(progn
       (cffi:defcfun ,(gsl-name "alloc") :pointer (size :size))
       (cffi:defcfun ,(gsl-name "calloc") :pointer (size :size))
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
	  ',(cl-name 'read 'formatted))))))

(export 'with-data)
(defmacro with-data ((type symbol size &optional zero) &body body)
  "Allocate GSL data, bind to pointer,
   and then deallocated it when done.  If zero is T, zero the
   contents when allocating."
  (flet ((cl-name (action)
	   (intern (format nil "GSL-~a-~a" type action))))
    `(let ((,symbol
	    (,(if zero (cl-name 'alloc) (cl-name 'alloc))
	      ,size)))
       (when (cffi:null-pointer-p ,symbol)
	 (error 'gsl-error
		:gsl-errno (cffi:foreign-enum-value 'gsl-errorno :ENOMEM)
		:gsl-reason
		(format nil "For '~a of GSL type ~a." ',symbol ',type)))
       (unwind-protect 
	    (progn ,@body)
	 (,(cl-name 'free) ,symbol)))))

#+prototype
(defmacro with-data ((type symbol size &optional zero) &body body)
  "Allocate GSL data, bind to pointer,
   and then deallocated it when done.  If zero is T, zero the
   contents when allocating."
  (flet ((cl-name (action)
	   (intern (format nil "GSL-~a-~a" type action))))
    (let ((ptr (gensym "PTR")))
      `(let ((,ptr
	      (,(if zero (cl-name 'alloc) (cl-name 'alloc))
		,size)))
	 (when (cffi:null-pointer-p ,ptr)
	   (error 'gsl-error
		  :gsl-errno (cffi:foreign-enum-value 'gsl-errorno :ENOMEM)
		  :gsl-reason
		  (format nil "For '~a of GSL type ~a." ',symbol ',type)))
	 (unwind-protect 
	      (progn ,@body)
	   (,(cl-name 'free) ,ptr))))))
