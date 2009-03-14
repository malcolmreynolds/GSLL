;; The mobject that defines callbacks
;; Liam Healy 2009-03-14 11:20:03EDT callback-included.lisp
;; Time-stamp: <2009-03-14 11:29:41EDT callback-included.lisp>
;; $Id: $

(in-package :gsl)

(defclass callback-included (mobject)
  ((cbstruct-name
    :initarg :cbstruct-name :reader cbstruct-name
    :documentation
    "The name of the GSL structure representing the callback(s).")
   (array-type
    :initarg :array-type :reader array-type
    :documentation "A symbol 'marray or 'cvector.")
   (callback-labels
    :initarg :callback-labels :reader callback-labels
    :documentation "The labels in the GSL struct for each callback function.")
   (dimension-names
    :initarg :dimension-names :reader dimension-names
    :documentation "The names in the GSL struct for dimensions.")
   (functions
    :initarg :functions :reader functions
    :documentation "The names of the function(s) put into
     the callback.  These should correspond in order to
     callback-labels.")
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
     cbstruct-name array-type callback-labels
     &optional (dimension-names '(dimensions)))
  `(defclass ,class-name ,superclasses
     ((cbstruct-name :initform ',cbstruct-name :allocation :class)
      (array-type :initform ',array-type :allocation :class)
      (callback-labels :initform ',callback-labels :allocation :class)
      (dimension-names :initform ',dimension-names :allocation :class))
     (:documentation ,documentation)))

(defmacro def-ci-subclass-1d
    (class-name superclasses documentation
     cbstruct-name array-type callback-labels)
  `(defclass ,class-name ,superclasses
     ((cbstruct-name :initform ',cbstruct-name :allocation :class)
      (array-type :initform ',array-type :allocation :class)
      (callback-labels :initform ',callback-labels :allocation :class)
      (dimension-names :initform nil :allocation :class)
      (dimensions :initform '(1) :allocation :class))
     (:documentation ,documentation)))


;;;;****************************************************************************
;;;; For making mobjects
;;;;****************************************************************************

#|
(defun ci-subclass-cdddr-args (callbacks)
  "Make the list
     cbstruct-name array-type callback-labels
     &optional (dimension-names '(dimensions)
   for def-ci-subclass and def-ci-subclass-1d."
  (list
   (parse-callback-static callbacks 'callback-structure-type)
   (callback-argument-function-argspec  'array-type))
  
  )
|#
