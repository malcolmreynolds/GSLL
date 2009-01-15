;; Mapping of element type names
;; Liam Healy 2008-04-13 11:22:46EDT element-types.lisp
;; Time-stamp: <2009-01-13 21:31:12EST element-types.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Common element type groups for generic functions
;;;;****************************************************************************

(defparameter *array-element-types*
  (remove-duplicates (all-types *cstd-cl-type-mapping* t) :test 'equal)
  "All the array element types supported.")

(defparameter *array-element-types-no-complex*
  (remove-if (lambda (tp) (subtypep tp 'complex)) *array-element-types*)
  "All the array element types supported except for complex types.")

(defparameter *float-complex-types*
  (remove-if (lambda (tp) (subtypep tp 'integer)) *array-element-types*)
  "All the float or complex array element types supported.")

(defparameter *float-types*
  (remove-if-not (lambda (tp) (subtypep tp 'float)) *array-element-types*)
  "All the float array element types.")

(defparameter *complex-types*
  (remove-if-not (lambda (tp) (subtypep tp 'complex)) *array-element-types*)
  "All the supported complex array element types.")

(defparameter *double-types*
  (list 'double-float '(complex double-float))
  "All the supported double float element types.")

(defun element-types (element-types)
  "All the element types available of a given category."
  (case element-types
    ((nil t) *array-element-types*)
    (:no-complex *array-element-types-no-complex*)
    (:float *float-types*)
    (:complex *complex-types*)
    (:float-complex *float-complex-types*)
    (:doubles *double-types*)
    (t element-types)))

