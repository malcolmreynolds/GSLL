;********************************************************
; file:        number-conversion.lisp
; description: Conversion of numbers C->CL
; date:        Sun May 28 2006 - 22:04                   
; author:      Liam M. Healy                             
; modified:    Mon Jan  1 2007 - 12:10
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Complex numbers
;;;;****************************************************************************

(cffi:defcstruct gsl-complex
  "A complex number in GSL."
  (dat :double :count 2))

(defun complex-to-cl (gsl-complex &optional (index 0))
  "Make a CL complex number from the GSL pointer to a complex struct or
   an array of complex structs and an index into the array." 
  (let ((carr (cffi:foreign-slot-value
	       (cffi:inc-pointer
		gsl-complex
		(* index (cffi:foreign-type-size 'gsl-complex)))
	       'gsl-complex 'dat)))
    (complex (mem-aref carr :double 0)
	     (mem-aref carr :double 1))))

;;;;****************************************************************************
;;;; Built-in numerical types
;;;;****************************************************************************

(defun float-to-cl (float &optional (index 0))
  (cffi:mem-aref float :float index))

(defun double-to-cl (double &optional (index 0))
  (cffi:mem-aref double :double index))

(defun size-to-cl (size &optional (index 0))
  (cffi:mem-aref size :size index))

(defun int-to-cl (integer &optional (index 0))
  (cffi:mem-aref integer :int index))

(defun cl-convert-function (type)
  (case type
    (:float 'float-to-cl)
    (:double 'double-to-cl)
    (:size 'size-to-cl)
    (:int 'int-to-cl)
    (gsl-complex 'complex-to-cl)))

(defparameter *make-sequence-type* 'list
  "Whether sequences should be returned as list or vector.")

(defun items-in-sequence (element-function length)
  "Make a CL sequence of the type specified by *make-sequence-type*,
   computing each element with the function element-function."
  (let ((ans (make-sequence *make-sequence-type* length)))
    (dotimes (i length ans)
      (setf (elt ans i)
	    (funcall element-function i)))))

(defun cl-convert-form (decl)
  "Generate a form that calls the appropriate converter from C/GSL to CL."
  (if (st-arrayp decl)		; eventually, check that it's a vector
      `((map *make-sequence-type*
	     (function
	      ,(cl-convert-function (st-eltype decl)))
	     (cffi::foreign-array-to-lisp
	      ,(st-symbol decl)
	      ',(st-eltype decl)
	      (list ,(st-dim decl)))))
      (case (st-type decl)
	(sf-result 
	 `((val ,(st-symbol decl))
	   (err ,(st-symbol decl))))
	(sf-result-e10
	 `((val ,(st-symbol decl) 'sf-result-e10)
	   (e10 ,(st-symbol decl))
	   (err ,(st-symbol decl) 'sf-result-e10)))
	(t `((,(cl-convert-function (st-type decl)) ,(st-symbol decl)))))))

