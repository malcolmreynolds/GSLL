;; The mobject that defines callbacks
;; Liam Healy 2009-03-14 11:20:03EDT callback-included.lisp
;; Time-stamp: <2009-03-14 21:22:18EDT callback-included.lisp>
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
;;;; Record the :callbacks information for the object
;;;;****************************************************************************

(defvar *callbacks-for-classes* (make-hash-table :size 32)
  "A table of :callbacks arguments for each class.")

(defun record-callbacks-for-class (class callbacks)
  (setf (gethash class *callbacks-for-classes*) callbacks))

(defun get-callbacks-for-class (class)
  (gethash class *callbacks-for-classes*))

;;;;****************************************************************************
;;;; Make defmcallback forms
;;;;****************************************************************************

;;; These functions make interned symbols that will be bound to
;;; dynamic variables.

(defun make-mobject-defmcallbacks (callbacks class)
  "Make the defmcallback forms needed to define the callbacks
   associated with mobject that includes callback functions."
  (let ((numcb (number-of-callbacks callbacks)))
    (make-defmcallbacks
     callbacks
     (mobject-cbvnames class numcb)
     (mobject-fnvnames class numcb))))

(defun mobject-variable-name (class-name suffix &optional count)
  (intern (format nil "~:@(~a~)-~:@(~a~)~@[~d~]" class-name suffix count)
	  :gsll))

(defun mobject-cbvname (class-name &optional count)
  (mobject-variable-name class-name 'cbfn count))

(defun mobject-cbvnames (class-name &optional count)
  (loop for i from 0 below count collect (mobject-cbvname class-name i)))

(defun mobject-fnvname (class-name &optional count)
  (mobject-variable-name class-name 'dynfn count))

(defun mobject-fnvnames (class-name &optional count)
  (when class-name
    (loop for i from 0 below count collect (mobject-fnvname class-name i))))

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
