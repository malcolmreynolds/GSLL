;; The mobject that defines callbacks
;; Liam Healy 2009-03-14 11:20:03EDT callback-included.lisp
;; Time-stamp: <2009-03-22 00:02:37EDT callback-included.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Class definitions and macros
;;;;****************************************************************************

(defclass callback-included (mobject)
  ((callbacks
    :initarg :callbacks :reader callbacks
    :documentation "The specification form for static callback information.")
   (dimension-names
    :initarg :dimension-names :reader dimension-names
    :documentation "The names in the GSL struct for dimensions.")
   (functions
    :initarg :functions :reader functions :initform nil
    :documentation "The names of the function(s) defined by defmcallback.
    These should correspond in order to the structure-slot-name list.")
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

;;;;****************************************************************************
;;;; For making mobjects
;;;;****************************************************************************

(defun select-dynamic-values (object n)
  `(,(nth n (functions object))
     ,(if (listp (scalarsp object))
	  (nth n (scalarsp object))
	  (scalarsp object))
     ,@(dimensions object)))

;;; This is expanded in defmfun to set the dynamic variables. 
(defun callback-set-dynamic (callback-object arglist)
  "Make a form to set the dynamic variable defining callbacks."
  `((setf 
     ,@(loop for symb in
	    (let ((class (category-for-argument arglist callback-object)))
	      (mobject-fnvnames
	       class
	       (number-of-callbacks (get-callbacks-for-class class))))
	    for n from 0
	    append `(,symb (select-dynamic-values ,callback-object ,n))))))
