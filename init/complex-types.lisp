;; Complex number types
;; Liam Healy 2009-01-13 21:24:05EST complex-types.lisp
;; Time-stamp: <2009-04-11 21:15:24EDT complex-types.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Complex types
;;;;****************************************************************************

;;; GSL represents complex variables as a struct of two reals and
;;; passes them by value (not a pointer) to functions.  In order to
;;; handle this we use FSBV.

;;; See /usr/include/gsl/gsl_complex.h
(fsbv:defcstruct complex-float-c
  (dat :float :count 2))

(fsbv:defcstruct complex-double-c
  (dat :double :count 2))

#+long-double
(fsbv:defcstruct complex-long-double-c
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

(defun component-float-type (eltype)
  "The type of the component of this type (complex)."
  (if (subtypep eltype 'complex)
      ;; complex: use the component type
      (second eltype)
      eltype))

(defun component-float-type-from-value (value)
  (let ((full (component-float-type (type-of value))))
    (if (listp full) (first full) full)))

;; See also complex-to-cl in number-conversion.lisp
(defun cl-to-complex (complex-number struct)
  "Set the CL complex number into the GSL struct."
  (let ((struct-type
	 (ecase (component-float-type-from-value complex-number)
	   (single-float 'complex-float-c)
	   (double-float 'complex-double-c))))
    (setf (cffi:mem-aref
	   (cffi:foreign-slot-value struct struct-type 'dat)
	   :double 0)
	  (realpart complex-number)
	  (cffi:mem-aref
	   (cffi:foreign-slot-value struct struct-type 'dat)
	   :double 1)
	  (imagpart complex-number))))

(define-condition pass-complex-by-value (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Cannot pass complex scalars to GSL functions.")))
  (:documentation
   "An error indicating that this implementation and platform are
   unable to pass complex numbers to the GSL libary by value."))

