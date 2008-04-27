;; Mapping of element type names
;; Liam Healy 2008-04-13 11:22:46EDT element-types.lisp
;; Time-stamp: <2008-04-26 21:36:27EDT element-types.lisp>
;; $Id$

;;; The different element type forms:
;;; FFA/CFFI             :uint8
;;; GSL splice name      "uchar"
;;; CL                   '(unsigned-byte 8)
;;; Single               'unsigned-byte-8 

;;; Functions to perform conversions
;;; CL -> single in function #'cl-single
;;; CL -> GSL in function #'cl-gsl
;;; CL -> FFA in function #'cl-ffa
;;; CFFI -> CL in function #'cffi-cl

;;; Sources of equivalence
;;; FFA -> CL in alist ffa::*cffi-and-lisp-types*
;;; FFA -> GSL in alist *ffa-gsl-type-mapping*

(in-package :gsl)

;;;;****************************************************************************
;;;; Basic definition
;;;;****************************************************************************

;;; FFA defines ffa::*cffi-and-lisp-types*
;;; Coordinate with cffi-grovel to get type names consistent.

;;; To be added to FFA, types that GSL defines:
;;; long, ulong (= :int64, :uint64 ?)
;;; long_double (is this extended float for intel?  SBCL doesn't support)
;;; complex, complex_float, complex_long_double
;;; GSL doesn't use "long long" but it's defined sometimes?

(defparameter *ffa-gsl-type-mapping*
  ;; Must be manually written with one pair for each of
  ;; (all-types ffa::*cffi-and-lisp-types*)
  '((:int8 . "char") (:uint8 . "uchar")
    (:int16 . "short") (:uint16 . "ushort")
    (:int32 . "int") (:uint32 . "uint")
    (:float . "float") (:double . ""))
  "An alist of an FFA type-name symbol and the GSL splice string.")

;;;;****************************************************************************
;;;; Conversions
;;;;****************************************************************************

(defun all-types (alist &optional right-side)
  "A list of all types defined by symbol or definition."
  (mapcar (if right-side #'rest #'first) alist))

(defun lookup-type (symbol alist &optional reverse)
  "Lookup the symbol defined in the alist."
  (if reverse
      (first (rassoc symbol alist :test #'equal))
      (rest (assoc symbol alist))))

;;; (cl-single '(unsigned-byte 8))
;;; UNSIGNED-BYTE-8
(defun cl-single (cl-type)
  "The element type name as a single symbol."
  (intern (if (atom cl-type)
	      (princ-to-string cl-type)
	      (format nil "~{~a~^-~}" cl-type))
	  :gsl))

;;; (cl-gsl '(unsigned-byte 8))
;;; "uchar"
(defun cl-gsl (cl-type &optional prepend-underscore)
  "The GSL splice string from the CL type."
  (let ((string
	 (lookup-type
	  (lookup-type cl-type ffa::*cffi-and-lisp-types* t)
	  *ffa-gsl-type-mapping*)))
    (if (and prepend-underscore (plusp (length string)))
	(concatenate 'string "_" string)
	string)))

(defun cl-ffa (cl-type)
  "The FFA/CFFI element type from the CL type."
  (lookup-type cl-type ffa::*cffi-and-lisp-types* t))

(defun cffi-cl (cffi-type)
  "The CL type from the FFA/CFFI element type."
  (lookup-type cffi-type ffa::*cffi-and-lisp-types*))

(defun splice-name (base-name type keyword)
  "Make a new C name for a data function from a base name."
  (let ((pos (search keyword base-name)))
    (when pos
      (let ((insert (+ pos (length keyword)))
	    (gsltype (cl-gsl type)))
	(concatenate 'string 
		     (subseq base-name 0 insert)
		     (if (zerop (length gsltype)) "" "_")
		     (cl-gsl type)
		     (subseq base-name insert))))))

;;;;****************************************************************************
;;;; Types for CFFI (will eventually be in CFFI)
;;;;****************************************************************************

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
