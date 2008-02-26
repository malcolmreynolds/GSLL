;; Definition of GSL objects and ways to use them.
;; Liam Healy, Sun Dec  3 2006 - 10:21
;; Time-stamp: <2008-02-18 11:46:20EST gsl-objects.lisp>
;; $Id$

(in-package :gsl)

;;; Each object has a defun named after it.  This function is used
;;; only internally by #'letm, but it is exported so that
;;; arglist prompters such as slime's will show the arguments
;;; needed.

(defparameter *letm-expanded-object* nil)

(defvar *not-for-users* nil)

(export 'letm)
(defmacro letm (bindings &body body)
  "A macro to bind variables to GSL objects, possibly setting
   initial values, and freeing them when done.  The binding is as
   in a let*; any bindings for non-GSL objects are done as in
   a let*."
  (let* (setters			; save setting forms
	 freers				; save freeing forms
	 assigns			; save global assignements
	 (*not-for-users* t))
    ;; Rewrite the bindings with allocators and push them on assigns
    (dolist (b bindings)
      (if (atom b)
	  (push b assigns)
	  (let ((defsymb (first b)) (defas (second b)))
	    (if (and (listp b) (listp defas)
		     (member (first defas) *letm-expanded-object*))
		;; It is a GSL object
		(destructuring-bind (allocator freer &optional setter assign)
		    (apply (first defas) (rest defas))
		  ;; If there's a setting function, save the form
		  (when setter (push (funcall setter defsymb) setters))
		  ;; Save the freeing form
		  (push `(,freer ,defsymb) freers)
		  ;; Save the global assignments
		  (when assign (push (funcall assign) assigns))
		  ;; Return the allocation form to be used in let
		  (push `(,defsymb ,allocator) assigns))
		;; Not a GSL object, just define it with let
		(push b assigns)))))
    (if freers
	;; There were GSL objects
	`(let* (,@(reverse assigns))
	  ;; put the body in an unwind-protect
	  (unwind-protect
	       (progn
		 ,@(reverse setters)		; setting the objects 
		 ,@body)
	    ,@freers))			; freeing the objects
	;; No GSL objects in the bindings, just make an ordinary let
	`(let* (,@bindings) ,@body))))

;;; It might be nice to have a letm* or something where one go is
;;; dependent on another's being initialized.

(defun not-for-users (form)
  (unless *not-for-users*
    (error "The form ~a should be placed in the binding of a letm."
	   form)))

;;; General definition for object creation
(defmacro defgo (symbol arglist &body body)
  "Define a GSL object to be bound in a letm.
   The body should return a list of two to four values.
   The first is the allocation form, the second is
   the freeing function name, the third is
   form to set or initialize the object,
   and the last is a function of no arguments
   that gives a form to be bound in a let prior
   to binding the object.  The symbol will be exported
   even though users should not call the function, so that
   it will show up in arglist prompters."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew ',symbol *letm-expanded-object*)
    (export ',symbol)
    (defun ,symbol ,arglist
      (not-for-users ',(cons symbol arglist))
      ,@body)))

;;; Specific, simple and common types of object creation 
(defmacro defgo-s (form allocate free &optional set (num-alloc-args 1))
  "Make an object usable in letm with the arglist is the allocate
   and set arglists appended."
  (let ((symb (gensym "SASF")))
    `(defgo ,(first form)
      (,@(subseq (rest form) 0 num-alloc-args)
       &optional
       ,@(when set `((,(nth num-alloc-args (rest form))
		      nil
		      settingp)))
       ,@(when set (subseq (rest form) (1+ num-alloc-args))))
      (list
       `(,',allocate ,,@(subseq (rest form) 0 num-alloc-args))
       ',free
       ,@(when set
	       `((when settingp
		   (lambda (,symb)
		     `(,',set ,,symb ,,@(subseq (rest form) num-alloc-args))))))))))
