;; Definitions for macro expansion
;; Liam Healy 2009-03-15 14:50:28EDT callback-compile-defs.lisp
;; Time-stamp: <2009-04-01 21:36:12EDT callback-compile-defs.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Record the :callbacks information for the object
;;;;****************************************************************************

(defvar *callbacks-for-classes* (make-hash-table :size 32)
  "A table of :callbacks arguments for each class.")

(defun record-callbacks-for-class (class callbacks)
  (setf (gethash class *callbacks-for-classes*) callbacks))

(defun get-callbacks-for-class (class)
  (gethash class *callbacks-for-classes*))

(defun make-cbstruct-object (class)
  "Make the callback structure based on the mobject definition."
  (let ((cbs (get-callbacks-for-class class)))
    (unless cbs (error "Class ~a not defined." class))
    `(make-cbstruct
      ',(parse-callback-static cbs 'callback-structure-type)
      (when (dimension-names object)
	(mappend 'list (dimension-names object) (dimensions object)))
      ,@(mappend
	 'list
	 (mapcar
	  (lambda (fn) `',(parse-callback-fnspec fn 'structure-slot-name))
	  (parse-callback-static cbs 'functions))
	 (mapcar (lambda (nm) `',nm)
		 (mobject-cbvnames class (number-of-callbacks cbs)))))))

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

