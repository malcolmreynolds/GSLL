;; Number types used by GSL functions, and specification conversion
;; Liam Healy 2008-12-31 21:06:34EST types.lisp
;; Time-stamp: <2009-04-29 22:48:31EDT types.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Unsigned address types size_t
;;;;****************************************************************************

(case
    (cffi:foreign-type-size :long)
  (8
   (push :int64 *features*)
   (cffi:defctype sizet :uint64)
   (fsbv:defsynonym sizet :uint64))
  (4
   (push :int32 *features*)
   (cffi:defctype sizet :uint32)
   (fsbv:defsynonym sizet :uint64))
  (t (error "Size of :long unrecognized")))

;;;;****************************************************************************
;;;; Type specification conversion
;;;;****************************************************************************

;; cffi-features:no-long-long doesn't work for me, but ought to be checked? 

;;; The different element type forms:
;;; C standard full, or "CFFI"  :unsigned-char
;;; GSL splice name             "uchar"
;;; C explicit                  :uint8     (not used here)
;;; CL                          '(unsigned-byte 8)
;;; Single                      'unsigned-byte-8 

;;; Functions to perform conversions
;;; CL -> single in function #'cl-single
;;; CL -> GSL in function #'cl-gsl
;;; CL -> CFFI (Cstd) in function #'cl-cffi
;;; CFFI (Cstd) -> CL in function #'cffi-cl

;;; Sources of equivalence
;;; Cstd -> CL in alist *cstd-cl-type-mapping*
;;; Cstd -> GSL in alist *cstd-gsl-mapping*

;;; :long-double may be pushed onto *features* if
;;; the implementation supports long doubles in CFFI.

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
    #+int64
    :long-long
    #+int64
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
   *fp-type-mapping*
   (mapcar
    (lambda (type)
      (cons
       type
       (list
	(if (string-equal type "uns" :end1 3)
	    'unsigned-byte
	    'signed-byte)
	(* 8 (cffi:foreign-type-size type)))))
    *cstd-integer-types*))
  ;; Be careful when reverse associating, as there may be several C
  ;; types that map to a single CL type.
  "An alist of the C standard types as keywords, and the CL type
   The exception is complex types, which don't have a definition
   in the C standard; in that case, the C type is the GSL struct
   definition.")

(defmacro floating-point-association (splice-list)
  `(mapcar
   #'cons
   (mapcar #'first *fp-type-mapping*)
   (subseq ,splice-list 0 (length *fp-type-mapping*))))

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
   (floating-point-association *gsl-splice-fp-types*))
  "Mapping the C standard types to the GSL splice name.")

(defparameter *blas-splice-fp-types*
  ;; Ordered by: real shortest to longest, then complex shortest to longest.
  ;; Must match *fp-type-mapping*.
  '("s" "d" #+long-double nil
    "c" "z" #+long-double nil)
  "The list of floating point types that can be spliced into BLAS function names.")

(defparameter *cstd-blas-mapping*
  ;; The floating types are associated by order, so it is important that
  ;; order of *fp-type-mapping* and *blas-splice-fp-types* match,
  ;; though the latter may be longer.
  (floating-point-association *blas-splice-fp-types*)
  "Mapping the C standard types to the BLAS splice name.")

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
(defun cl-gsl (cl-type &optional prepend-underscore blas)
  "The GSL splice string from the CL type."
  (let ((string
	 (lookup-type
	  (lookup-type cl-type *cstd-cl-type-mapping* t)
	  (if blas *cstd-blas-mapping* *cstd-gsl-mapping*))))
    (if (and prepend-underscore (plusp (length string)))
	(concatenate 'string "_" string)
	string)))

(defun cl-cffi (cl-type)
  "The CFFI element type from the CL type."
  (lookup-type (clean-type cl-type) *cstd-cl-type-mapping* t))

(defun cffi-cl (cffi-type)
  "The CL type from the CFFI element type."
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

