;; Definition of GSL objects and ways to use them.
;; Liam Healy, Sun Dec  3 2006 - 10:21
;; Time-stamp: <2009-08-25 20:45:31EDT mobject.lisp>

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

;;; Required arguments for defmobject:
;;;  class           Name of class being made.
;;;  prefix          String that starts each GSL function name
;;;  allocation-args Arguments used in the allocation (initialization)
;;;  description     Short string describing class

;;; Key arguments for defmobject:
;;; documentation
;;;  Docstring for the object maker.
;;; initialize-suffix
;;;  The string appended to prefix to form GSL function name or a list of
;;;  such a string and the c-return argument.
;;; initialize-name
;;;  A string with the name of the GSL function for initialization; defaults to
;;;  prefix + initialize-suffix.
;;; initialize-args
;;; arglists-function
;;;  A function of one symbol, a flag indicating that an optional
;;;  argument has been set.  It returns a list of three forms: the
;;;  arglists for the maker defun, the arguments that will be applied
;;;  therein to the inititializer and to the reinitializer.
;;; inputs, gsl-version
;;;  Defmfun arguments for reinitializer.
;;; allocator
;;; allocate-inputs
;;; freer
;;; callbacks
;;;   See callbacks.lisp
;;; superclasses
;;;   List of superclasses other than 'mobject.
;;; singular
;;;   Where a list of several objects like 'functions is specified, this permits
;;;   a single one with the singular form, like 'function.
;;; switch
;;;   Same as for defmfun.
;;; ri-c-return
;;;   The c-return for the reinitialize-instance function.

(defmacro defmobject
    (class prefix allocation-args description 
     &key documentation initialize-suffix initialize-name initialize-args
     arglists-function inputs gsl-version allocator allocate-inputs freer
     ((:callbacks cbinfo))
     (superclasses (if cbinfo '(callback-included) '(mobject)))
     singular switch ri-c-return)
  "Define the class, the allocate, initialize-instance and
   reinitialize-instance methods, and the make-* function for the GSL object."
  (let* ((settingp (make-symbol "SETTINGP"))
	 (arglists
	  (when arglists-function
	    (funcall (coerce arglists-function 'function) settingp)))
	 (maker (intern (format nil "MAKE-~:@(~a~)" class) :gsl))
	 (cl-alloc-args (variables-used-in-c-arguments allocation-args))
	 (cl-initialize-args
	  (callback-replace-arg
	   'functions
	   (variables-used-in-c-arguments
	    (if (initialize-suffix-switched-foreign initialize-suffix)
		(second initialize-args)
		initialize-args))
	   cbinfo))
	 (initializerp (or initialize-name initialize-suffix)))
    ;; Need callback information for macroexpansion make-cbstruct-object
    (when cbinfo (record-callbacks-for-class class cbinfo))
    (if (have-at-least-gsl-version gsl-version)
	`(progn
	   ,(if cbinfo
		`(,(if (member 'dimensions cl-alloc-args)
		       'def-ci-subclass 'def-ci-subclass-1d)
		   ,class
		   ,superclasses
		   ,(format nil "The GSL representation of the ~a." description)
		   ,(parse-callback-static cbinfo 'dimension-names))
		`(defclass ,class ,superclasses
		   nil
		   (:documentation
		    ,(format nil "The GSL representation of the ~a." description))))

	   (defmfun allocate ((object ,class) &key ,@cl-alloc-args)
	     ,(or allocator (format nil "~a_alloc" prefix))
	     ,allocation-args
	     ,@(if allocate-inputs `(:inputs ,allocate-inputs))
	     :definition :method
	     :c-return :pointer
	     :index ,maker)

	   ,(make-initialize-instance
	     class cl-alloc-args cl-initialize-args prefix freer)
	   ,@(when initializerp
		   (make-reinitialize-instance
		    class cl-initialize-args initialize-name prefix
		    initialize-suffix initialize-args inputs
		    cbinfo
		    superclasses
		    switch ri-c-return))
	   (export '(,maker ,class))
	   ,@(when cbinfo `((record-callbacks-for-class ',class ',cbinfo)))
	   ,@(when cbinfo (make-mobject-defmcallbacks cbinfo class))
	   ,(mobject-maker
	     maker arglists class cl-alloc-args cl-initialize-args
	     description documentation initialize-args initializerp settingp
	     singular cbinfo))
	`(progn
	   (export ',maker)
	   (defun ,maker (&rest args)
	     (declare (ignore args))
	     (error 'obsolete-gsl-version
		    :name ',class :gsl-name ,prefix
		    :gsl-version ',gsl-version))))))

(defun initialize-suffix-switched-foreign (initialize-suffix)
  "The specified initialize-suffix indicates that there are two
   foreign functions that can be called; which one is called
   is dependent on the presence or absense of certain arguments."
  (and initialize-suffix (listp initialize-suffix)))

(defun make-initialize-instance
    (class cl-alloc-args cl-initialize-args prefix freer)
  `(defmethod initialize-instance :after
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
		     ,(or freer (format nil "~a_free" prefix))
		     :pointer mpointer :void)))))

(defun make-reinitialize-instance
    (class cl-initialize-args initialize-name prefix
     initialize-suffix initialize-args inputs
     cbinfo superclasses switch ri-c-return)
  "Expand the reinitialize-instance form."
  (flet ((add-suffix (suffix) (format nil "~a_~a" prefix suffix)))
    (let* ((cbstruct (make-symbol "CBSTRUCT"))
	   (initialize-suffix-switched-foreign
	    (initialize-suffix-switched-foreign initialize-suffix)))
      `((defmfun reinitialize-instance
	    ((object ,class)
	     &key
	     ,@cl-initialize-args
	     ,@(when cbinfo
		     `(&aux (,cbstruct ,(make-cbstruct-object class)))))
	  ,(or initialize-name
	       (if initialize-suffix-switched-foreign
		   (mapcar #'add-suffix initialize-suffix)
		   (add-suffix initialize-suffix)))
	  ,(if initialize-suffix-switched-foreign
	       (mapcar (lambda (ia)
			 `(((mpointer object) :pointer)
			   ,@(callback-replace-arg cbstruct ia cbinfo)))
		       initialize-args)
	       `(((mpointer object) :pointer)
		 ,@(callback-replace-arg cbstruct initialize-args cbinfo)))
	  :definition :method
	  ,@(when cbinfo '(:callback-object object))
	  :qualifier :after
	  ,@(when ri-c-return `(:c-return ,ri-c-return))
	  ,@(when switch (list :switch switch))
	  :return (object)
	  ,@(when inputs `(:inputs ,inputs))
	  ,@(when cbinfo
		  `(:before
		    (,@(when (member 'callback-included-cl superclasses)
			     `((setf (slot-value object 'callback) ,cbstruct)))
		       (make-funcallables-for-object object))
		    :after
		    ((trivial-garbage:finalize 
		      object
		      (lambda ()
			(cffi:foreign-free ,cbstruct))))))
	  :export nil
	  :index (reinitialize-instance ,class))))))

(defun mobject-maker
    (maker arglists class cl-alloc-args cl-initialize-args
     description documentation initialize-args initializerp settingp
     singular cbinfo)
  "Make the defun form that makes the mobject."
  (when cbinfo
    (setf cl-initialize-args (append cl-initialize-args '((scalarsp t)))))
  (let ((initargs ; arguments that are exclusively for reinitialize-instance
	 (remove-if (lambda (s) (member s cl-alloc-args)) cl-initialize-args)))
    `(defun ,maker
	 ,(if arglists
	      (first arglists)
	      (singularize
	       singular
	       `(,@cl-alloc-args
		 ,@(when initargs
			 (append
			  (list
			   '&optional
			   (if (listp (first initargs))
			       `(,@(first initargs) ,settingp)
			       `(,(first initargs) nil ,settingp)))
			  (rest initargs))))))
       ,(format
	 nil "Create the GSL object representing a ~a (class ~a).~@[~&~a~]"
	 description class documentation)
       (let ((object
	      (make-instance
	       ',class
	       ,@(when cbinfo `(:cbinfo ',cbinfo))
	       ,@(if arglists
		     (second arglists)
		     (symbol-keyword-symbol cl-alloc-args singular)))))
	 ;; There is an initialization step
	 ,@(when initializerp
		 (if (or initialize-args arglists)	; with arguments
		     (let ((reii 
			    `(reinitialize-instance
			      object
			      ,@(if arglists
				    (third arglists)
				    (symbol-keyword-symbol
				     (arglist-plain-and-categories cl-initialize-args)
				     singular)))))
		       (if initargs `((when ,settingp ,reii)) `(,reii)))
		     '((reinitialize-instance object)))) ; without arguments
	 object))))

(defun plural-symbol (symbol)
  "Make the plural form of this symbol."
  (intern (format nil "~aS" (symbol-name symbol))
	  (find-package :gsl)))

(defun singular-symbol (symbol)
  "Make the singular form of this symbol."
  (let ((string (symbol-name symbol)))
    (if (eq #\S (aref string (1- (length string))))
	(intern (subseq string 0 (1- (length string)))
		(find-package :gsl)))))

(defun singularize (symbols form)
  "In the form, replace the plural symbol with the singular symbol
   given."
  (if symbols
      (if (listp symbols)
	  (singularize (first symbols) (singularize (rest symbols) form))
	  (subst symbols (plural-symbol symbols) form))
      form))

(defun symbol-keyword-symbol (symbol &optional singular)
  "Make a list of key symbol, listifying if singular."
  (if (listp symbol)
      (mappend (lambda (s) (symbol-keyword-symbol s singular)) symbol)
      (if (member (singular-symbol symbol) singular)
	  (list (intern (symbol-name symbol) :keyword)
		`(list ,(singular-symbol symbol)))
	  (list (intern (symbol-name symbol) :keyword)
		symbol))))

;;;;****************************************************************************
;;;; Generic functions
;;;;****************************************************************************

(defgeneric allocate (object &key &allow-other-keys)
  (:documentation
   "Use GSL to allocate memory.  Returns pointer but does not bind mpointer slot."))

(export 'name)
(defgeneric name (object)
  (:documentation "The name given to the GSL object."))

(export 'size)
(defgeneric size (object)
  (:documentation "The size of the GSL object.")
  (:method ((object array))
    (array-total-size object)))

(export 'evaluate)
(defgeneric evaluate (object point &key #+sbcl &allow-other-keys)
  (:documentation "Evaluate the GSL object."))

;;; Pointer type
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +foreign-pointer-class+ (class-name (class-of (cffi:null-pointer)))
    "The class in which foreign pointers fall.")
  (defconstant +foreign-pointer-type+ (type-of (cffi:null-pointer))
    "The type of foreign pointers."))

;;; Note: clisp has a foreign pointer class = T, so it would be best
;;; to narrow down methods that dispatch on this class.
(defmacro foreign-pointer-method (pointer form)
  "Execute this form only if the pointer is of +foreign-pointer-type+;
   otherwise call the next method."
  #-clisp (declare (ignore pointer))
  #+clisp
  `(if (typep ,pointer +foreign-pointer-type+)
       ,form
       (call-next-method))
  #-clisp
  form)

;;; Some functions in solve-minimize-fit return a pointer to a GSL
;;; vector with double-floats.  This function will return a contents
;;; form suitable for make-marray.  There is no choice but to copy
;;; over the data even on native implementations; because GSL is doing
;;; the mallocing, the data are not CL-accessible.

(defmethod mpointer ((object #.+foreign-pointer-class+))
  #+clisp (check-type object #.+foreign-pointer-type+)
  object)

(export 'order)
(defgeneric order (object)
  (:documentation "The order of the GSL object."))

;;;;****************************************************************************
;;;; Making objects from existing objects
;;;;****************************************************************************

(export 'copy)
(defun copy (object &optional destination)
  "Create a duplicate object."
  (if destination
      (copy-to-destination object destination)
      (copy-making-destination object)))

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
	  (if (typep object 'marray)
	      (call-next-method)
	      (make-instance (class-of object) :mpointer (call-next-method)))
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

