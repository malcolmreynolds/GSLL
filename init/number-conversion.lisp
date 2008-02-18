;; Conversion of numbers C->CL
;; Liam Healy, Sun May 28 2006 - 22:04
;; Time-stamp: <2008-02-17 18:54:32EST number-conversion.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Built-in numerical types
;;;;****************************************************************************

(export '(fcref dcref scref icref))

(defmacro fcref (float &optional (index 0))
  "Reference C float(s)."
  `(cffi:mem-aref ,float :float ,index))

(defmacro dcref (double &optional (index 0))
  "Reference C double(s)."
  `(cffi:mem-aref ,double :double ,index))

(defmacro scref (size &optional (index 0))
  "Reference C size(s)."
  `(cffi:mem-aref ,size 'size ,index))

(defmacro icref (integer &optional (index 0))
  "Reference C integer(s)."
  `(cffi:mem-aref ,integer :int ,index))

(defun cl-convert-function (type)
  (case type
    (:float 'fcref)
    (:double 'dcref)
    (size 'scref)
    (:int 'icref)
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
    (complex (dcref carr 0)
	     (dcref carr 1))))

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

