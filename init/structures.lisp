;********************************************************
; file:        structures.lisp                           
; description: Common structures defined by GSL.         
; date:        Sun May 28 2006 - 22:04                   
; author:      Liam M. Healy                             
; modified:    Mon Jun 26 2006 - 21:38
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Result from special functions
;;;;****************************************************************************

(cffi:defcstruct sf-result
  "Results from special functions with value and error estimate."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
  (val :double)
  (err :double))

(cffi:defcstruct sf-result-e10
  "Results from special functions with value, error estimate
and a scaling exponent e10, such that the value is val*10^e10."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
  (val :double)
  (err :double)
  (e10 :int))

(cffi:defcenum sf-mode
  "Numerical precision modes with which to calculate special functions."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC62
  :double-prec
  :single-prec
  :approx-prec)

(defun val (sf-result &optional (type 'sf-result))
  (cffi:foreign-slot-value sf-result type 'val))

(defun err (sf-result &optional (type 'sf-result))
  (cffi:foreign-slot-value sf-result type 'err))

(defun e10 (sf-result)
  (cffi:foreign-slot-value sf-result 'sf-result-e10 'e10))

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

;;; obsolete; just do this manually for the few cases of multiple sf-result returns?
(defun rearrange-sf-result-err (return-list)
  "Put the 'err values from the sf-results at the end of the return values."
  (flet ((sf-err (x)
	   (and (eq (first x) 'cffi:foreign-slot-value)
		(member (third x) '('sf-result 'sf-result-e10) :test #'equal)
		(equal (fourth x) ''err))))
    (append
     (remove-if #'sf-err return-list)
     (remove-if-not #'sf-err return-list))))
