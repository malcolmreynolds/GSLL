;; Definition of GSL objects and ways to use them.
;; Liam Healy, Sun Dec  3 2006 - 10:21
;; Time-stamp: <2008-12-25 23:01:24EST gsl-objects.lisp>
;; $Id$

(in-package :gsl)

#|
(defclass mobject ()
  ((mpointer :initarg :mpointer :reader mpointer
	     :documentation "A pointer to the GSL representation of the object.")))


(defmacro defmobject
    (class prefix allocation-args description 
     &optional docstring initialize-suffix initialize-args arglists-function
     inputs)
  "Define the class, initialize-instance and reinitialize-instance methods,
   and the make-* function for the GSL object."
  ;; If prefix is a list, the first is the actual prefix, and the
  ;; second is the name of the allocateor.  I'm looking at you,
  ;; discrete-random.  Grrr.
  (let* ((settingp (make-symbol "SETTINGP"))
	 (arglists
	  (when arglists-function
	    (funcall (coerce arglists-function 'function) settingp)))
	 (maker (intern (format nil "MAKE-~:@(~a~)" class) :gsl))
	 (cl-alloc-args (variables-used-in-c-arguments allocation-args))
	 (cl-initialize-args
	  (variables-used-in-c-arguments initialize-args))
	 (realprefix (if (listp prefix) (first prefix) prefix)))
    `(progn
       (defclass ,class (mobject)
	 ()
	 (:documentation
	  ,(format nil "The GSL representation of the ~a." description)))

       (defmfun allocate ((object ,class) &key ,@cl-alloc-args)
	 ,(if (listp prefix) (second prefix) (format nil "~a_alloc" prefix))
	 ,allocation-args
	 :definition :method
	 :c-return :pointer
	 :index ,maker)

       (defmethod initialize-instance :after
	   ((object ,class) &key mpointer ,@cl-alloc-args)
	 (unless mpointer
	   (setf mpointer
		 (allocate object ,@(symbol-keyword-symbol cl-alloc-args))))
	 (trivial-garbage:finalize
	  object
	  (lambda ()
	    (cffi:foreign-funcall
	     ,(format nil "~a_free" realprefix)
	     :pointer mpointer :void))))

       ,@(when initialize-suffix
	       `((defmfun reinitialize-instance
		     ,(if initialize-args
			  `((object ,class) &key ,@cl-initialize-args)
			  `((object ,class)))
		   ,(format nil "~a_~a" realprefix initialize-suffix)
		   (((mpointer object) :pointer) ,@initialize-args)
		   :definition :method
		   :qualifier :after
		   :return (object)
		   ,@(when inputs `(:inputs ,inputs))
		   :export nil
		   :index (reinitialize-instance ,class))))

       (export ',maker)
       (defun ,maker
	   ,(if arglists
		(first arglists)
		`(,@cl-alloc-args
		  ,@(when initialize-args
			  (append
			   (list '&optional
				 (list (first cl-initialize-args) nil settingp))
			   (rest cl-initialize-args)))))
	 ,(format nil "Create the GSL object representing a ~a (class ~a).~@[~&~a~]"
		  description class docstring)
	 (let ((object
		(make-instance
		 ',class
		 ,@(if arglists
		       (second arglists)
		       (symbol-keyword-symbol cl-alloc-args)))))
	   ;; There is an initialization step
	   ,@(when initialize-suffix
		   (if initialize-args	; with arguments
		       `((when ,settingp
			   (reinitialize-instance
			    object
			    ,@(if arglists
				  (third arglists)
				  (symbol-keyword-symbol cl-initialize-args)))))
		       '((reinitialize-instance object)))) ; without arguments
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

(defgeneric allocate (object &key &allow-other-keys)
  (:documentation
   "Use GSL to allocate memory.  Returns pointer but does not bind mpointer slot."))

;;; Could be part of copy when the second argument is optional?
(export 'clone)
(defgeneric clone (object)
  (:documentation "Create a duplicate object.")
  (:method :around ((object mobject))
	   (make-instance (class-of object) :mpointer (call-next-method))))

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
