;; Conversion of numbers C->CL
;; Liam Healy, Sun May 28 2006 - 22:04
;; Time-stamp: <2008-03-09 14:17:34EDT number-conversion.lisp>
;; $Id$

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

;;;;****************************************************************************
;;;; Types
;;;;****************************************************************************

;;; CL type definitions:
(deftype unsigned-byte8 () '(unsigned-byte 8))
(deftype unsigned-byte16 () '(unsigned-byte 16))
(deftype unsigned-byte32 () '(unsigned-byte 32))
(deftype unsigned-byte64 () '(unsigned-byte 64))

;;; C type definitions from cffi-net/unixint.cffi.lisp:
(cffi:defctype uint8 :unsigned-char)
(cffi:defctype uint16 :unsigned-short)
(cffi:defctype uint32 :unsigned-int)
(cffi:defctype uint64 :unsigned-long)
(cffi:defctype int8 :char)
(cffi:defctype int16 :short)
(cffi:defctype int32 :int)
(cffi:defctype int64 :long)

#-cffi-features:no-long-long
(cffi:defctype size uint64)

#+cffi-features:no-long-long
(progn (cerror "Use :uint32 instead." "This platform does not support long long types.")
       (cffi:defctype size uint32))

;;;(deftype unsigned-fixnum () `(integer 0 ,most-positive-fixnum))

(defparameter *type-names*
  '(
    (fixnum :int "_int")
    (unsigned-fixnum :unsigned-int "_uint")
    (single-float :float "_float")
    (double-float :double "")
    ;;(long-double :long "_long_double")
    (complex gsl-complex "_complex")
    (t nil ""))
  "A list of type name mappings, each being a list
   Lisp type, C type as CFFI understands it, and GSL
   splice string.")

(defun lookup-splice-name (cl-type)
  (third (find cl-type *type-names* :key #'first)))

(defun lookup-C-type (cl-type)
  (second (find cl-type *type-names* :key #'first)))

(defun splice-name (base-name keyword symbol)
  "Make a new C name for a data function from a base name." 
  (let* ((insert (+ (search keyword base-name) (length keyword))))
    (concatenate 'string
		 (subseq base-name 0 insert)
		 (lookup-splice-name symbol)
		 (subseq base-name insert))))
