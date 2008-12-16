;; Definition of GSL objects and ways to use them.
;; Liam Healy, Sun Dec  3 2006 - 10:21
;; Time-stamp: <2008-12-15 18:21:19EST gsl-objects.lisp>
;; $Id$

(in-package :gsl)

#|
(defclass gsl-object ()
  ((mpointer :accessor mpointer
	     :documentation "A pointer to the GSL representation of the object.")))

(defmacro defmobject
    (class prefix allocation-args description
     &optional initialize-suffix initialize-args)
  (let ((mptr (make-symbol "MPOINTER"))
	(maker (intern (format nil "MAKE-~:@(~a~)" class) :gsl))
	(cl-alloc-args
	 (variables-used-in-c-arguments allocation-args))
	(cl-initialize-args
	 (variables-used-in-c-arguments initialize-args))
	(settingp (make-symbol "SETTINGP")))
    `(progn
       (defclass ,class (gsl-object)
	 ()
	 (:documentation
	  ,(format nil "The GSL representation of the ~a." description)))

       (defmfun initialize-instance ((object ,class) &key ,@cl-alloc-args)
	 ,(format nil "~a_alloc" prefix)
	 ,allocation-args
	 :definition :method
	 :qualifier :after
	 :c-return (,mptr :pointer)
	 :after ((setf (slot-value object 'mpointer) ,mptr)
		 (tg:finalize
		  object
		  (lambda ()
		    (foreign-funcall ,(format nil "~a_free" prefix) :pointer ,mptr :void))))
	 :return (object)
	 :export nil
	 :index ,maker)

       (defmfun reinitialize-instance ((object ,class) &key ,@cl-initialize-args)
	 ,(format nil "~a_~a" prefix initialize-suffix)
	 (((mpointer object) :pointer) ,@initialize-args)
	 :definition :method
	 :qualifier :after
	 :return (object)
	 :export nil
	 :index (reinitialize-instance ,class))

       (export ',maker)
       (defun ,maker
	   (,@cl-alloc-args
	    &optional
	    ,@(cons (list (first cl-initialize-args) nil settingp)
		    (rest cl-initialize-args)))
	 ,(format nil "Create the GSL object representing a ~a (class ~a)."
		  description class)
	 (let ((object
		(make-instance ',class ,@(symbol-keyword-symbol cl-alloc-args))))
	   (when ,settingp
	     (reinitialize-instance
	      object
	      ,@(symbol-keyword-symbol cl-initialize-args)))
	   object)))))

(defun symbol-keyword-symbol (symbol)
  (if (listp symbol)
      (mapcan #'symbol-keyword-symbol symbol)
      (list (intern (symbol-name symbol) :keyword)
	    symbol)))

;;; To make-load-form
;;; (make-basis-spline (bspline-order bspline) (number-of-breakpoints bspline))
;;; Then initialize by making a vector from (breakpoint i bspline)
;;; and calling knots.
|#

;;;;;;;;;;;;;;;;;;;;;;;; OLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; There are numerous "GSL objects" which need to by manually
;;; memory-managed.  The macro letm takes care of this.
;;; A method #'letm-expansion which eql specializes on the object name
;;; in the second argument is responsible for doing the expansion.
;;;
;;; Many GSL objects simply need to be allocated, optionally set, and
;;; then freed after the work has been done.  To cover those cases,
;;; the macro #'defgo (and simpler macro #'defgo-s) is defined.  The
;;; default #'letm-expansion assumes that the object has been defined
;;; with defgo or defgo-s.  Their names are saved in
;;; *letm-expanded-object*.
;;; 
;;; Each object has a defun named after it, with an arglist
;;; (and perhaps docstring) appropriate for when that form
;;; is a binding to a letm.  This function shouldn't be called;
;;; it is defined and exported for the benefit of arglist
;;; displayers like slime.


(export 'letm)
(defmacro letm (bindings &body body)
  (first (letm-expand bindings body)))

(defun letm-expand (bindings body)
  (if (null bindings)
      body
      (let ((binding (first bindings)))
	(if (and (listp binding) (listp (second binding)))
	    (destructuring-bind (symbol (object &rest arguments))
		binding
	      `(,(letm-expansion
		  symbol
		  object
		  arguments
		  (letm-expand (rest bindings) body))))
	    ;; binding value is an atom or not present
	    `((let (,binding)
		,@(letm-expand (rest bindings) body)))))))

(defvar *not-for-users* nil)
(defparameter *letm-expanded-object* nil)

(defun not-for-users (form)
  (unless *not-for-users*
    (error "The form ~a should be placed in the binding of a letm."
	   form)))

(defmacro arglist-only (symbol docstring &rest arglist)
  "Define objects to be used with letm bindings, but 
   make the function itself signal an error."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew ',symbol *letm-expanded-object*)
    (export ',symbol)
    (defun ,symbol (,@arglist) ,docstring
	   (declare (ignorable ,@arglist))
	   (not-for-users (list ',symbol ,@arglist)))))

(defgeneric letm-expansion (symbol type args body)
  ;; The method that expands a defgo-created object.
  (:method (symbol type args body)
    (if (member type *letm-expanded-object*)
	(let ((*not-for-users* t))
	  (destructuring-bind (allocator freer &optional setter assign)
	      (if type (apply type args) (list nil nil))
	    `(let* (,@(when assign (list (funcall assign)))
		    (,symbol ,allocator))
	      (unwind-protect
		   (progn
		     ,@(when setter (list (funcall setter symbol)))
		     ,@body)
		(,freer ,symbol)))))
	;; Not a GSL object; create a regular let form
	`(let ((,symbol (,type ,@args)))
	  ,@body))))

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

;;; Pointer type
(defconstant +foreign-pointer-class+ (class-name (class-of (cffi:null-pointer)))
  "The class in which foreign pointers fall.  This will be assumed to be a 
   GSL vector or matrix.")

;;; Generic allocation and free used for histograms and random
(export '(alloc free))
(defgeneric alloc (object)
  (:documentation "Allocate the GSL storage."))
(defgeneric free (object)
  (:documentation "Free the GSL storage."))
