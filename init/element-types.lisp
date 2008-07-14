;; Mapping of element type names
;; Liam Healy 2008-04-13 11:22:46EDT element-types.lisp
;; Time-stamp: <2008-07-12 14:21:59EDT element-types.lisp>
;; $Id$

;;; The different element type forms:
;;; C standard full      :unsigned-char
;;; GSL splice name      "uchar"
;;; C explicit           :uint8     (not used here)
;;; CL                   '(unsigned-byte 8)
;;; Single               'unsigned-byte-8 

;;; Functions to perform conversions
;;; CL -> single in function #'cl-single
;;; CL -> GSL in function #'cl-gsl
;;; CL -> Cstd in function #'cl-ffa
;;; CFFI (Cstd) -> CL in function #'cffi-cl

;;; Sources of equivalence
;;; Cstd -> CL in alist *cstd-cl-type-mapping*
;;; Cstd -> GSL in alist *cstd-gsl-mapping*

;;; :long-double may be pushed onto *features* if
;;; the implementation supports long doubles in CFFI.

(in-package :gsl)

;;;;****************************************************************************
;;;; Basic definition
;;;;****************************************************************************

;;; Preliminary definitions.  These are not used directly outside this
;;; file; they exist to define the two variables
;;; *cstd-cl-type-mapping* and *cstd-gsl-mapping* which are used by
;;; the conversion functions.

(defparameter *cstd-integer-types*
  '(:char :unsigned-char
    :short :unsigned-short
    :int :unsigned-int
    :long :unsigned-long
    #-cffi-features:no-long-long
    :long-long
    #-cffi-features:no-long-long
    :unsigned-long-long)
  ;; http://common-lisp.net/project/cffi/manual/html_node/Built_002dIn-Types.html
  "List of integer types supported by CFFI, from the CFFI docs.")

(defparameter *fp-type-mapping*
  '((:float . single-float) (:double . double-float)
    ;; For those implementations that support a separate long-double
    ;; type assume this mapping:
    #+long-double (:long-double . long-float)
    (complex-float-c . (complex single-float))
    (complex-double-c . (complex double-float))
    ;; For those implementations that support a separate long-double
    ;; type assume this mapping:
    #+long-double
    (complex-long-double-c . (complex long-float)))
  ;; Ordered by: real shortest to longest, then complex shortest to longest.
  "List of floating point types supported by CFFI from the CFFI docs
   plus corresponding complex types.")

(defparameter *gsl-splice-int-types*
  ;; list | grep -i 'gsl_vector.*_alloc\b'
  '("char" "int" "long" "short" "uchar" "uint" "ulong" "ushort")
  "The list of integer types that can be spliced into function names.")

(defparameter *gsl-splice-fp-types*
  ;; list | grep -i 'gsl_vector.*_alloc\b'
  ;; Ordered by: real shortest to longest, then complex shortest to longest.
  ;; Must match *fp-type-mapping*.
  '("float" "" #+long-double "long_double"
    "complex_float" "complex" #+long-double "complex_long_double")
  "The list of floating point types that can be spliced into function names.")

;;; Mapping alists used by conversion functions.

;;; Used by #'cl-gsl
(defparameter *cstd-cl-type-mapping*
  (append
   (mapcar
    (lambda (type)
      (cons
       type
       (list
	(if (string-equal type "uns" :end1 3)
	    'unsigned-byte
	    'signed-byte)
	(* 8 (cffi:foreign-type-size type)))))
    *cstd-integer-types*)
   *fp-type-mapping*)
  ;; Be careful when reverse associating, as there may be several C
  ;; types that map to a single CL type.
  "An alist of the C standard types as keywords, and the CL type
   The exception is complex types, which don't have a definition
   in the C standard; in that case, the C type is the GSL struct
   definition.")

(defparameter *cstd-gsl-mapping*
  (append
   ;; The integer types 
   (remove-if-not
    (lambda (x) (find (rest x) *gsl-splice-int-types* :test 'string-equal))
    (mapcar
     (lambda (type)
       (cons type
	     (let ((ut
		    (if (string-equal type "uns" :end1 3)
			(string-downcase
			 (concatenate 'string "u" (subseq (string type) 9)))
			(string-downcase type))))
	       (if (and (> (length ut) 8)
			(string-equal
			 (subseq ut (- (length ut) 9))
			 "long-long"))
		   (concatenate 'string (subseq ut 0 (- (length ut) 9)) "llong")
		   ut))))
     *cstd-integer-types*))
   ;; The floating types are associated by order, so it is important that
   ;; order of *fp-type-mapping* and *gsl-splice-fp-types* match,
   ;; though the latter may be longer.
   (mapcar
    #'cons
    (mapcar #'first *fp-type-mapping*)
    (subseq *gsl-splice-fp-types* 0 (length *fp-type-mapping*))))
  "Mapping the C standard types to the GSL splice name.")

;;;;****************************************************************************
;;;; Conversions
;;;;****************************************************************************

(defun all-types (alist &optional right-side)
  "A list of all types defined by symbol or definition."
  (mapcar (if right-side #'rest #'first) alist))

(defun lookup-type (symbol alist &optional reverse)
  "Lookup the symbol defined in the alist."
  (or 
   (if reverse
       (first (rassoc symbol alist :test #'equal))
       (rest (assoc symbol alist)))
   ;;(error "Did not find ~a in ~a" symbol (mapcar #'first alist))
   ))

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
	  (lookup-type cl-type *cstd-cl-type-mapping* t)
	  *cstd-gsl-mapping*)))
    (if (and prepend-underscore (plusp (length string)))
	(concatenate 'string "_" string)
	string)))

(defun cl-ffa (cl-type)
  "The FFA/CFFI element type from the CL type."
  (lookup-type (clean-type cl-type) *cstd-cl-type-mapping* t))

(defun cffi-cl (cffi-type)
  "The CL type from the FFA/CFFI element type."
  (unless (eq cffi-type :pointer)
    (lookup-type cffi-type *cstd-cl-type-mapping*)))

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
;;;; GSL complex types
;;;;****************************************************************************

;;; GSL defines complex numbers in a struct, and passes the struct by
;;; value.  CFFI does not support call by value for structs, so we
;;; cannot use functions that call or return complex scalars.

;;; See /usr/include/gsl/gsl_complex.h
(cffi:defcstruct complex-float-c
  (dat :float :count 2))

(cffi:defcstruct complex-double-c
  (dat :double :count 2))

#+long-double
(cffi:defcstruct complex-long-double-c
  (dat :long-double :count 2))

(defun clean-type (type)
  ;; SBCL at least will specify limits on the type, e.g.
  ;; (type-of #C(1.0 2.0))
  ;; (COMPLEX (DOUBLE-FLOAT 1.0 2.0))
  ;; This cleans that up to make
  ;; (clean-type (type-of #C(1.0 2.0)))
  ;; (COMPLEX DOUBLE-FLOAT)
  (if (and (subtypep type 'complex) (listp (second type)))
      (list (first type) (first (second type)))
      type))

(defun complex-to-gsl (number gsl)
  "Set the already-allocated GSL (foreign) struct to the CL complex number.
   Returns the struct."
  (cerror "Accept gibberish."
	  "Cannot pass complex scalars to and from GSL functions (structs passed by value).")
  (let* ((cleantype (clean-type (type-of number)))
	 (comptype (cl-ffa (second cleantype)))
	 (datslot
	  (cffi:foreign-slot-pointer gsl (cl-ffa cleantype) 'dat)))
    (setf (cffi:mem-aref datslot comptype 0) (realpart number)
	  (cffi:mem-aref datslot comptype 1) (imagpart number))
    gsl))

;;; Use GSL to create the complex.  This actually does work on
;;; SBCL/amd64, but it shouldn't.
#+(or)
(defmfun complex-to-gsl (complex)
  "gsl_complex_rect"
  (((realpart complex) :double) ((imagpart complex) :double))
  :c-return :pointer
  :documentation
  "Convert the CL complex double into a GSL complex.")

#|
(defmfun complex-abs (number)
  "gsl_complex_abs"
  (((complex-to-gsl number) :pointer))
  :c-return :double)

(defmfun complex-arg (number)
  "gsl_complex_arg"
  (((complex-to-gsl number) :pointer))
  :c-return :double)
|#

;;;;****************************************************************************
;;;; Element types
;;;;****************************************************************************

(defparameter *array-element-types*
  (remove-duplicates (all-types *cstd-cl-type-mapping* t) :test 'equal)
  ;;(all-types ffa::*cffi-and-lisp-types* t)
  "All the array element types supported.")

(defparameter *array-element-types-no-complex*
  (remove-if (lambda (tp) (subtypep tp 'complex)) *array-element-types*)
  "All the array element types supported except for complex types.")

;;;;****************************************************************************
;;;; Types for CFFI (will eventually be in CFFI)
;;;;****************************************************************************

(defvar *sizet-type* nil
  "The CL type for size_t.")

(case
  (cffi:foreign-type-size :long)
  (8
   (push :sizet-64 *features*)
   (setf *sizet-type* '(unsigned-byte 64))
   (cffi:defctype sizet :uint64))
  (4
   (push :sizet-32 *features*)
   (setf *sizet-type* '(unsigned-byte 32))
   (cffi:defctype sizet :uint32))
  (t (error "Size of :long unrecognized")))
