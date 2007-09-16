;********************************************************
; file:        number-conversion.lisp
; description: Conversion of numbers C->CL
; date:        Sun May 28 2006 - 22:04                   
; author:      Liam M. Healy                             
; modified:    Sat Sep 15 2007 - 22:34
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Built-in numerical types
;;;;****************************************************************************

(defmacro float-to-cl (float &optional (index 0))
  `(cffi:mem-aref ,float :float ,index))

(defmacro double-to-cl (double &optional (index 0))
  `(cffi:mem-aref ,double :double ,index))

(defmacro size-to-cl (size &optional (index 0))
  `(cffi:mem-aref ,size :size ,index))

(defmacro int-to-cl (integer &optional (index 0))
  `(cffi:mem-aref ,integer :int ,index))

(defun cl-convert-function (type)
  (case type
    (:float 'float-to-cl)
    (:double 'double-to-cl)
    (:size 'size-to-cl)
    (:int 'int-to-cl)
    (gsl-complex 'complex-to-cl)))

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
    (complex (double-to-cl carr 0)
	     (double-to-cl carr 1))))

;;;;****************************************************************************
;;;; Conversion form
;;;;****************************************************************************

(defun cl-convert-form (decl)
  "Generate a form that calls the appropriate converter from C/GSL to CL."
  (case (st-type decl)
    (sf-result 
     `((val ,(st-symbol decl))
       (err ,(st-symbol decl))))
    (sf-result-e10
     `((val ,(st-symbol decl) 'sf-result-e10)
       (e10 ,(st-symbol decl))
       (err ,(st-symbol decl) 'sf-result-e10)))
    (t `((,(cl-convert-function (st-type decl)) ,(st-symbol decl))))))

