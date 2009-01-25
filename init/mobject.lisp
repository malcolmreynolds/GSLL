;; Definition of GSL objects and ways to use them.
;; Liam Healy, Sun Dec  3 2006 - 10:21
;; Time-stamp: <2009-01-25 10:35:08EST mobject.lisp>
;; $Id$

;;; GSL objects are represented in GSLL as and instance of a 'mobject.
;;; The macro demobject takes care of defining the appropriate
;;; subclass, the methods #'initialize-instance,
;;; #'reinitialize-instance, and #'allocate, and the function
;;; #'make-<class-name>, which is what the user calls to make the
;;; object.

(in-package :gsl)

;;;;****************************************************************************
;;;; The class definition
;;;;****************************************************************************

(defclass mobject ()
  ((mpointer :initarg :mpointer :reader mpointer
	     :documentation "A pointer to the GSL representation of the object.")))

(defmacro defmobject
    (class prefix allocation-args description 
     &key documentation initialize-suffix initialize-args arglists-function
     inputs gsl-version)
  "Define the class, the allocate, initialize-instance and
   reinitialize-instance methods, and the make-* function for the GSL object."
  ;; If prefix is a list, the first is the actual prefix, and the
  ;; second is the name of the allocator.  I'm looking at you,
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
    (if (have-at-least-gsl-version gsl-version)
	`(progn
	   (defclass ,class (mobject)
	     ()
	     (:documentation
	      ,(format nil "The GSL representation of the ~a." description)))

	   (defmfun allocate ((object ,class) &key ,@cl-alloc-args)
	     ,(if (listp prefix) (second prefix) (format nil "~a_alloc" prefix))
	     ,allocation-args
	     ,@(if (and (listp prefix) (third prefix))
		   `(:inputs ,(third prefix)))
	     :definition :method
	     :c-return :pointer
	     :index ,maker)

	   (defmethod initialize-instance :after
	       ((object ,class) &key mpointer ,@(union cl-alloc-args cl-initialize-args))
	     ,@(let ((not-alloc (set-difference cl-initialize-args cl-alloc-args)))
		    (when not-alloc)
		    `((declare (ignore ,@not-alloc))))
	     (unless mpointer
	       (setf mpointer
		     (allocate object ,@(symbol-keyword-symbol cl-alloc-args))
		     (slot-value object 'mpointer) mpointer))
	     (tg:finalize object
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
	     ,(format
	       nil "Create the GSL object representing a ~a (class ~a).~@[~&~a~]"
	       description class documentation)
	     (let ((object
		    (make-instance
		     ',class
		     ,@(if arglists
			   (second arglists)
			   (symbol-keyword-symbol cl-alloc-args)))))
	       ;; There is an initialization step
	       ,@(when initialize-suffix
		       (if initialize-args ; with arguments
			   `((when ,settingp
			       (reinitialize-instance
				object
				,@(if arglists
				      (third arglists)
				      (symbol-keyword-symbol cl-initialize-args)))))
			   '((reinitialize-instance object)))) ; without arguments
	       object)))
	`(defun ,maker (&rest args)
	   (declare (ignore args))
	   (error 'obsolete-gsl-version
		  :name ',class :gsl-name ,realprefix
		  :gsl-version ',gsl-version)))))

(defun symbol-keyword-symbol (symbol)
  (if (listp symbol)
      (mapcan #'symbol-keyword-symbol symbol)
      (list (intern (symbol-name symbol) :keyword)
	    symbol)))

;;;;****************************************************************************
;;;; Generic functions
;;;;****************************************************************************

(defgeneric allocate (object &key &allow-other-keys)
  (:documentation
   "Use GSL to allocate memory.  Returns pointer but does not bind mpointer slot."))

(export 'name)
(defgeneric name (object)
  (:documentation "The name given to the GSL object."))

;;; Pointer type
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +foreign-pointer-class+ (class-name (class-of (cffi:null-pointer)))
    "The class in which foreign pointers fall.")
  (defconstant +foreign-pointer-type+ (type-of (cffi:null-pointer))
    "The type of foreign pointers."))

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
  (check-type object #.+foreign-pointer-type+)
  object)

;;;;****************************************************************************
;;;; Making objects from existing objects
;;;;****************************************************************************

(export 'copy)
(defgeneric copy (object &optional destination)
  (:documentation "Create a duplicate object.")
  ;; This method is used for all GSL objects except arrays.
  (:method ((object mobject) &optional destination)
    (if destination
	(copy-to-destination object destination)
	(copy-making-destination object))))

(defgeneric copy-to-destination (object destination)
  ;; This copies values into an existing object.  Methods are defined
  ;; for non-array GSL objects that have _memcpy functions defined.
  ;; Defined for
  ;; random-number-generator, quasi-random-number-generator,
  ;; histogram, histogram2d, permutation, combination.
  (:documentation "Copy contents into existing object."))

(defgeneric copy-making-destination (object)
  (:documentation "Create new duplicate object.")
  (:method :around ((object mobject))
    (if (next-method-p)
	;; The subclass method should only return the malloced
	;; mpointer (as from a "_clone" function); it will be put into
	;; the CL object here.  The initial-instance method for these
	;; objects must be written to do nothing if the mpointer is
	;; already defined.
	;; Defined for
	;; histogram, histogram2d, 
	;; random-number-generator, quasi-random-number-generator,
	(make-instance (class-of object) :mpointer (call-next-method))
	;; The subclass does not supply a method, so this will be called
	;; by default.  We can only try to make something from the load form.
	(eval (make-load-form object)))))

;;; Could be part of copy when the second argument is optional?
(export 'clone)
(defgeneric clone (object)
  (:documentation "Create a duplicate object.")
  (:method :around ((object mobject))
	   (make-instance (class-of object) :mpointer (call-next-method))))


;;; To make-load-form
;;; (make-basis-spline (bspline-order bspline) (number-of-breakpoints bspline))
;;; Then initialize by making a vector from (breakpoint i bspline)
;;; and calling knots.

