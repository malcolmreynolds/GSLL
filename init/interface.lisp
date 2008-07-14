;; Macros to interface GSL functions, including definitions necessary for defmfun.
;; Liam Healy 
;; Time-stamp: <2008-07-10 21:39:43EDT interface.lisp>
;; $Id$

(in-package :gsl)

(export '(gsl-lookup *make-sequence-type*))

;;;;****************************************************************************
;;;; Lookup table to find CL functions from C function names
;;;;****************************************************************************

(defparameter *gsl-symbol-equivalence*
  (make-hash-table :test 'equal :size 1500))

(defun map-name (cl-name gsl-name)
  ;; Trust here that the library does not have two symbols that differ
  ;; only by the case of one or more letters.
  (setf (gethash (string-downcase gsl-name) *gsl-symbol-equivalence*) cl-name))

(defun gsl-lookup (string)
  "Find the GSLL (Lisp) equivalent of the GSL symbol."
  (gethash (string-downcase string) *gsl-symbol-equivalence*))

;;;;****************************************************************************
;;;; Symbol-type declaration
;;;;****************************************************************************

;;; An "st" or symbol-type is a list (symbol type) where
;;; type could be (element-type array-dim).  These are examples of lists
;;; of sts: 
 ;; ((#:RET3500 SF-RESULT))
 ;; ((#:RET3501 (:DOUBLE (- NMAX NMIN)))) 
 ;; ((#:RET3502 (:DOUBLE (1+ KMAX))) (#:RET3503 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3504 (:DOUBLE (1+ KMAX))) (#:RET3505 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3506 :DOUBLE) (#:RET3507 :DOUBLE))

(defun make-st (symbol type)
  (list symbol type))

(defun st-symbol (decl)
  (first decl))

(defun st-type (decl)
  (second decl))

(defun st-arrayp (decl)
  (listp (st-type decl)))

(defun st-array-pointer-last-p (decl)
  (listp (st-type decl)))

(defun st-eltype (decl)
  (first (st-type decl)))

(defun st-dim (decl)
  (second (st-type decl)))

(defun st-pointer-last-p (decl)
  (third (st-type decl)))

(defun wfo-declare (d)
  `(,(st-symbol d)
    ,@(if (st-arrayp d)
	  `(',(st-eltype d) ,(st-dim d))
	  `(',(st-type d)))))

;;;;****************************************************************************
;;;; Checking results from GSL functions
;;;;****************************************************************************

(defun check-gsl-status (status-code context)
  "Check the return status code from a GSL function and signal a warning
   if it is not :SUCCESS."
  (unless (eql status-code success)
    (signal-gsl-warning status-code (format nil "in ~a" context))))

(defun check-null-pointer (pointer error-code reason)
  (when (cffi:null-pointer-p pointer)
    (signal-gsl-error error-code reason)))

(defun success-failure (value)
  "If status indicates failure, return NIL, othewise return T."
  ;;(not (eql value (cffi:foreign-enum-value 'gsl-errorno :FAILURE)))
  ;; More general, to allow :CONTINUE
  (not (minusp value)))

(defun success-continue (value)
  "If status indicates success, return T, othewise return NIL."
  (eql value success))

;;;;****************************************************************************
;;;; Argument check
;;;;****************************************************************************

(defun cl-symbols (arglist)
  "The symbols in the arglist."
  (mapcar (lambda (s) (if (listp s) (first s) s)) arglist))

;;; (cl-argument-types '(a b) '((a :double) (b :int32)))
(defun cl-argument-types (cl-arguments c-arguments-types)
  "Create CL argument and types from the C arguments."
  (loop for sd in c-arguments-types
	for cl-type = (cffi-cl (st-type sd))
	append
	(when (and cl-type (member (st-symbol sd) (cl-symbols cl-arguments)))
	  (list (list (st-symbol sd) cl-type)))))

(defun declaration-form (cl-argument-types)
  (cons 'declare
	(mapcar (lambda (v) (cons 'type (reverse v))) cl-argument-types)))

;;;;****************************************************************************
;;;; Returns
;;;;****************************************************************************

(defvar *special-c-return*
  '(:error-code :number-of-answers :success-failure :success-continue
    :true-false :enumerate))

;;;;****************************************************************************
;;;; Variables in library
;;;;****************************************************************************

(defmacro defmpar (cl-symbol gsl-symbol documentation)
  "Define a library variable pointer."
  `(progn
    (cffi:defcvar (,gsl-symbol ,cl-symbol :read-only t) :pointer
      ,documentation)
    (map-name ',cl-symbol ,gsl-symbol)
    (export ',cl-symbol)))

;;;;****************************************************************************
;;;; GSL library version
;;;;****************************************************************************

(cffi:defcvar ("gsl_version" *gsl-version* :read-only t) :string
          "The version of the GSL library being used.")
(map-name '*gsl-version* "gsl_version")
(export '*gsl-version*)
