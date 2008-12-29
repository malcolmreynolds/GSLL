;; Definition of GSL objects and ways to use them.
;; Liam Healy, Sun Dec  3 2006 - 10:21
;; Time-stamp: <2008-12-28 18:25:33EST mobject.lisp>
;; $Id$

;;; GSL objects are represented in GSLL as and instance of a 'mobject.
;;; The macro demobject takes care of defining the appropriate
;;; subclass, the methods #'initialize-instance,
;;; #'reinitialize-instance, and #'allocate, and the function
;;; #'make-<class-name>, which is what the user calls to make the
;;; object.

(in-package :gsl)

(defclass mobject ()
  ((mpointer :initarg :mpointer :reader mpointer
	     :documentation "A pointer to the GSL representation of the object.")))

(defmacro defmobject
    (class prefix allocation-args description 
     &optional docstring initialize-suffix initialize-args arglists-function
     inputs)
  "Define the class, the allocate, initialize-instance and
   reinitialize-instance methods, and the make-* function for the GSL object."
  ;; If prefix is a list, the first is the actual prefix, and the
  ;; second is the name of the allocateor.  I'm looking at you,
  ;; discrete-random.  Grrr.
  ;; Argument 'initialize-suffix: string appended to prefix for form GSL function name
  ;;   or a list of such a string and the c-return argument.
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
		 (allocate object ,@(symbol-keyword-symbol cl-alloc-args))
		 (slot-value object 'mpointer) mpointer))
	 (trivial-garbage:finalize
	  object
	  (lambda ()
	    (cffi:foreign-funcall
	     ,(format nil "~a_free" realprefix)
	     :pointer mpointer :void))))

       ,@(when initialize-suffix
	       `((defmfun reinitialize-instance
		     ((object ,class) &key ,@cl-initialize-args)
		   ,(format nil "~a_~a" realprefix
			    (if (listp initialize-suffix)
				(first initialize-suffix)
				initialize-suffix))
		   (((mpointer object) :pointer) ,@initialize-args)
		   :definition :method
		   :qualifier :after
		   ,@(when (listp initialize-suffix)
			   `(:c-return ,(second initialize-suffix)))
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

(defgeneric allocate (object &key &allow-other-keys)
  (:documentation
   "Use GSL to allocate memory.  Returns pointer but does not bind mpointer slot."))

;;; Could be part of copy when the second argument is optional?
(export 'clone)
(defgeneric clone (object)
  (:documentation "Create a duplicate object.")
  (:method :around ((object mobject))
	   (make-instance (class-of object) :mpointer (call-next-method))))

(export 'name)
(defgeneric name (object)
  (:documentation "The name given to the GSL object."))

;;; Pointer type
(defconstant +foreign-pointer-class+ (class-name (class-of (cffi:null-pointer)))
  "The class in which foreign pointers fall.  This will be assumed to be a 
   GSL vector or matrix.")

;;; Some functions in solve-minimize-fit return a pointer to a GSL
;;; vector with double-floats.  This function will return a contents
;;; form suitable for make-marray.  There is no choice but to copy
;;; over the data even on native implementations; because GSL is doing
;;; the mallocing, the data are not CL-accessible.

(defgeneric contents-from-pointer (pointer struct-type &optional element-type)
  (:documentation
   "Create a contents list from the GSL object of type struct-type
    referenced by pointer."))

(defmethod mpointer ((object #.+foreign-pointer-class+))
  object)
