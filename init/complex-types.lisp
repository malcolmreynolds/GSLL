;; Complex number types
;; Liam Healy 2009-01-13 21:24:05EST complex-types.lisp
;; Time-stamp: <2009-01-14 22:45:53EST complex-types.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Complex types
;;;;****************************************************************************

;;; GSL represents complex variables as a struct of two reals and
;;; passes them by value (not a pointer) to functions.  Few FFIs have
;;; the capability of handling that.  However, it does seem to work
;;; (SBCL and CCL at least) that we can treat each complex argument as
;;; a succession of two double-float arguments, or two single-floats
;;; packed into a double float.  Here we probe that the first is true;
;;; if so, functions will automatically be converted.
(defparameter *pass-complex-scalar-as-two-reals*
  (eql 5.0d0
   (ignore-errors
     (cffi:foreign-funcall "gsl_complex_abs" :double 3.0d0 :double 4.0d0 :double)))
  "A complex number can be passed as two adjacent reals.")

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

(defun component-float-type (eltype)
  "The type of the component of this type (complex)."
  (if (subtypep eltype 'complex)
      ;; complex: use the component type
      (second eltype)
      eltype))

(define-condition pass-complex-by-value (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Cannot pass complex scalars to GSL functions.")))
  (:documentation
   "An error indicating that this implementation and platform are
   unable to pass complex numbers to the GSL libary by value."))

(defun pack-complex-as-double (number)
  "Pack a number of type (complex single-float) into a double-float,
   which may be accepted as a struct of type gsl_complex_float.
   This is hideously non-portable."
  (integer-as-float
   (dpb
    (float-as-integer (imagpart number) t)
    (byte 32 32)
    (dpb
     (float-as-integer (realpart number) t)
     (byte 32 0)
     0))
   'double-float))

(defun passing-complex-by-value (cfind)
  "Substitution in defmfun so that complex numbers can be passed by
   value to the GSL structs defined above.  This is non-portable."
  (if (eq (component-float-type (cffi-cl (third cfind))) 'single-float)
      ;; single-float
      `(:double	(pack-complex-as-double ,(first cfind)))
      ;; double-float
      `(:double
	(realpart ,(first cfind))
	:double
	(imagpart ,(first cfind)))))
