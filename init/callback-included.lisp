;; The mobject that defines callbacks
;; Liam Healy 2009-03-14 11:20:03EDT callback-included.lisp
;; Time-stamp: <2009-03-15 14:53:27EDT callback-included.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Class definitions and macros
;;;;****************************************************************************

(defclass callback-included (mobject)
  ((callbacks
    :initarg :callbacks :reader callbacks
    :documentation "The specification form for static callback information.")
   (callback-dynamic
    :initarg :callback-dynamic :reader callback-dynamic
    :documentation "A list of (function scalarsp dimensions &rest dimensions).")
   (dimension-names
    :initarg :dimension-names :reader dimension-names
    :documentation "The names in the GSL struct for dimensions.")
   (functions
    :initarg :functions :reader functions
    :documentation "The names of the function(s) defined by defmcallback.
    These should correspond in order to the structure-slot-name list.")
   (dimensions :initarg :dimensions :reader dimensions))
  (:documentation
   "A mobject that includes a callback function or functions to GSL."))

(defclass callback-included-cl (callback-included)
  ((callback :initarg :callback :reader callback-struct))
  (:documentation
   "A mobject that includes a callback function or functions, in which
    the pointer to the callback structure is stored in a CL class
    slot."))

(defmacro def-ci-subclass
    (class-name superclasses documentation
     &optional (dimension-names '(dimensions)))
  `(defclass ,class-name ,superclasses
     ((dimension-names :initform ',dimension-names :allocation :class))
     (:documentation ,documentation)))

(defmacro def-ci-subclass-1d
    (class-name superclasses documentation)
  `(defclass ,class-name ,superclasses
     ((dimension-names :initform nil :allocation :class)
      (dimensions :initform '(1) :allocation :class))
     (:documentation ,documentation)))

;;;;****************************************************************************
;;;; For making mobjects
;;;;****************************************************************************

(defun callback-set-dynamic (callback-object arglist)
  "Make a form to set the dynamic variable defining callbacks."
  `((setf 
     ,@(loop for symb in
	    (let ((class (category-for-argument arglist callback-object)))
	      (mobject-fnvnames
	       class
	       (number-of-callbacks (get-callbacks-for-class class))))
	    for n from 0
	    append
	    `(,symb (nth ,n (callback-dynamic ,callback-object))))))) 
