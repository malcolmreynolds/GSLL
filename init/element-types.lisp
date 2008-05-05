;; Mapping of element type names
;; Liam Healy 2008-04-13 11:22:46EDT element-types.lisp
;; Time-stamp: <2008-05-04 23:06:29EDT element-types.lisp>
;; $Id$

;;; The different element type forms:
;;; C standard full      :unsigned-char
;;; GSL splice name      "uchar"
;;; C explicit           :uint8
;;; CL                   '(unsigned-byte 8)
;;; Single               'unsigned-byte-8 

;;; Functions to perform conversions
;;; CL -> single in function #'cl-single
;;; CL -> GSL in function #'cl-gsl
;;; CL -> explicit in function #'cl-ffa
;;; CFFI -> CL in function #'cffi-cl

;;; Sources of equivalence
;;; explicit -> CL in alist ffa::*cffi-and-lisp-types*
;;; explicit -> GSL in alist *ffa-gsl-type-mapping*

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
    (:long-double . long-float)
    (:complex-float . (complex single-float))
    (:complex-double . (complex double-float))
    ;; For those implementations that support a separate long-double
    ;; type assume this mapping:
    (:complex-long-double . (complex long-float)))
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
  '("float" "" "long_double"
    "complex_float" "complex" "complex_long_double")
  "The list of floating point types that can be spliced into function names.")

;;; Mapping alists used by conversion functions.

;;; Used by #'cl-gsl
;;; Will replace ffa::*cffi-and-lisp-types*
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
  "An alist of the C standard types as keywords, and the CL type.")

;;; Will replace *ffa-gsl-type-mapping*
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

;;;========== TO BE REMOVED, will be obsolete ================

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

#-cffi-features:no-long-long
(cffi:defctype size :uint64)

#+cffi-features:no-long-long
(progn (cerror "Use :uint32 instead." "This platform does not support long long types.")
       (cffi:defctype size :uint32))
