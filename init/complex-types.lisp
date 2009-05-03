;; Complex number types
;; Liam Healy 2009-01-13 21:24:05EST complex-types.lisp
;; Time-stamp: <2009-05-03 10:00:23EDT complex-types.lisp>
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
#+fsbv
(fsbv:defcstruct
    (complex-float-c :constructor complex :deconstructor (realpart imagpart))
  (dat :float :count 2))

#-fsbv
(cffi:defcstruct complex-float-c (dat :float :count 2))

#+fsbv
(fsbv:defcstruct
    (complex-double-c :constructor complex :deconstructor (realpart imagpart))
  (dat :double :count 2))

#-fsbv
(cffi:defcstruct complex-double-c (dat :double :count 2))

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

