;; The mobject that defines callbacks
;; Liam Healy 2009-03-14 11:20:03EDT callback-included.lisp
;; Time-stamp: <2009-04-04 21:45:12EDT callback-included.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Class definitions and macros
;;;;****************************************************************************

(defclass callback-included (mobject)
  ((cbinfo
    :initarg :cbinfo :reader cbinfo
    :documentation "The specification form for static callback information.")
   (dimension-names
    :initarg :dimension-names :reader dimension-names
    :documentation "The names in the GSL struct for dimensions.")
   (functions
    :initarg :functions :reader functions :initform nil
    :documentation "The user functions as function designators.
    These should correspond in order to the structure-slot-name list.")
   (funcallables
    :initarg :funcallables :reader funcallables :initform nil
    :documentation "The function objects that will be called by
    the callbacks.")
   (scalarsp
    :initarg :scalarsp :reader scalarsp :initform t
    :documentation "Whether the function expect to be passed and return
    scalars or arrays.")
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
    (class-name superclasses documentation dimension-names)
  `(defclass ,class-name ,superclasses
     ((dimension-names :initform ',dimension-names :allocation :class))
     (:documentation ,documentation)))

(defmacro def-ci-subclass-1d
    (class-name superclasses documentation ignore)
  (declare (ignore ignore))
  `(defclass ,class-name ,superclasses
     ((dimension-names :initform nil :allocation :class)
      (dimensions :initform '(1) :allocation :class)
      (scalarsp :initform T :allocation :class))
     (:documentation ,documentation)))

(defmethod print-object ((object callback-included) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'functions)
      (princ "for " stream)
      (princ (first (functions object)) stream)
      (princ ", " stream))
    (princ "dimensions " stream)
    (princ (dimensions object) stream)))

;;;;****************************************************************************
;;;; For making mobjects
;;;;****************************************************************************

;;; This is expanded in defmfun to set the dynamic variables. 
(defun callback-set-dynamic (callback-object &optional arglist)
  "Make a form to set the dynamic variable defining callbacks."
  (when (listp callback-object)
    (setf arglist callback-object
	  callback-object (caar callback-object)))
  `((setf 
     ,@(loop for symb in
	    (let ((class (category-for-argument arglist callback-object)))
	      (mobject-fnvnames
	       class
	       (number-of-callbacks (get-callbacks-for-class class))))
	    for n from 0
	    append `(,symb (nth ,n (funcallables ,callback-object)))))))
