;; Foreign callback functions.               
;; Liam Healy 
;; Time-stamp: <2009-03-05 22:48:19EST callback.lisp>
;; $Id$

(in-package :gsl)

;;; Callback functions are functions which are passed as data; to Lisp
;;; that means they are just functions, but C makes a distinction.
;;; They are needed by several GSL tasks.  Functions that take one
;;; double-float and return one double-float and are defined using the
;;; gsl_function structure (see gsl_math.h) are called scalar
;;; functions.  They are used in numerical-integration,
;;; numerical-differentiation, chebyshev and definitions to aid in
;;; creating and using them are provided here.  The idea behind the
;;; definitions is that they absorb us much of the definition overhead
;;; as possible so that the user just defines a CL function and calls
;;; a macro to generate something to pass to the GSL functions.

;;; Other GSL tasks make use of callback functions with different
;;; characteristics.  Since they are specific to each of the tasks,
;;; they are defined with those tasks.  The macro #'defmcallback can
;;; specify that the CL function is to expect in arglist and return as
;;; multiple values scalar quantities that come from and will be bound
;;; to either marrays or C vectors.  This is done with a list of the
;;; type and size, e.g. (:double 3), and for setting :set, type size,
;;; e.g. (:set :double 3).  If the 'marray argument is nil, it will
;;; expand to read or set a C vector; if it is T, it will expand to
;;; read or set a marray.  This allows the user to define ordinary CL
;;; functions with scalars as input and output.  However, it may be
;;; desirable to read and set marrays, in which case :pointer is the
;;; right specification.

;;;;****************************************************************************
;;;; Setting slots
;;;;****************************************************************************

(defun set-structure-slot (foreign-structure structure-name slot-name value)
  (setf (cffi:foreign-slot-value foreign-structure structure-name slot-name)
	value))

(defun set-slot-function (foreign-structure structure-name slot-name gsl-function)
  (set-structure-slot
   foreign-structure structure-name slot-name
   (cffi:get-callback gsl-function)))

(defun set-parameters (foreign-structure structure-name)
  "Set the parameters slot to null."
  (set-structure-slot foreign-structure structure-name
		      'parameters (cffi:null-pointer)))

;;;;****************************************************************************
;;;; The GSL struct for holding callbacks
;;;;****************************************************************************

(cffi:defcstruct gsl-function
  "Passing functions to GSL."
  ;; see /usr/include/gsl/gsl_math.h
  (function :pointer)
  (parameters :pointer))

;;;;****************************************************************************
;;;; Macro defmcallback OBSOLETE
;;;;****************************************************************************

;;; Callback functions are defined using demcallback by passing the
;;; name of the function and the argument list of types.  Arrays can
;;; be handled in one of two ways.  If they are declared :pointer, the
;;; CL function will be passed a C pointer, and it is responsible for
;;; reading or setting the array, with #'dcref or #'maref.  If they
;;; are declared (type size) then size scalars will be passed as
;;; arguments to the CL function, and if they are declared (:set type
;;; size), the CL function return size values, to which the array
;;; elements will be set.

;;; Usage example for scalar function (e.g. numerical-integration,
;;; numerical-differentiation, chebyshev, ntuple).  
;;; (defmcallback myfn :double :double)
;;; Usage example for vector function (e.g. roots-multi)
;;; (defmcallback myfn :pointer :int (:pointer))
;;; Usage example for function and derivative
;;; (defmcallback fdf :pointer :double (:pointer :pointer))
;;; (defmcallback fdf :success-failure :int (:pointer :pointer))
;;; Usage example for def-ode-functions
;;; (defmcallback vanderpol :success-failure (:double (:double 2) (:set :double 2)))
;;; or
;;; (defmcallback vanderpol :success-failure (:double :pointer :pointer))
;;; to read and set within the CL function with #'dcref.

;;; (callback-args '(:double (:double 2) (:set :double 2)))
;;; ((#:ARG1193 :DOUBLE) (#:ARG1194 :POINTER) (#:ARG1195 :POINTER))

;;; (embedded-clfunc-args '(:double (:double 2) (:set :double 2)) (callback-args '(:double (:double 2) (:set :double 2))))
;;; (#:ARG1244 (MEM-AREF #:ARG1245 ':DOUBLE 0) (MEM-AREF #:ARG1245 ':DOUBLE 1))

(defvar *setting-spec* '(:set))
(defun embedded-clfunc-args (types callback-args &optional marray)
  "The arguments passed to the CL function call embedded in the callback.
   If 'marray is T, then reference GSL arrays; otherwise reference raw
   C vectors.  A specification (:set ...) means that the CL function
   will define the array as multiple values; if the size is negative,
   then the opposite value will be used for marray."
  (loop for spec in types
     for (symbol nil) in callback-args
     append
     (unless (and (listp spec) (member (first spec) *setting-spec*))
       (if (listp spec)
	   (if (third spec)
	       ;; matrix, marrays only
	       (loop for i from 0 below (second spec)
		  append
		  (loop for j from 0 below (third spec)
		     collect
		     `(maref ,symbol ,i ,j ',(cffi-cl (first spec)))))
	       ;; vector, marray or C array
	       (loop for ind from 0 below (abs (second spec))
		  collect (if (if (minusp (second spec)) (not marray) marray)
			      `(maref ,symbol ,ind nil ',(cffi-cl (first spec)))
			      `(cffi:mem-aref ,symbol ',(first spec) ,ind))))
	   (list symbol)))))

(defun callback-set-mvb (form types callback-args &optional marray)
  "Create the multiple-value-bind form in the callback to set the return C arrays."
  (multiple-value-bind (settype setcba)
      (loop for cba in callback-args
	 for type in types
	 for setting = (and (listp type) (member (first type) *setting-spec*))
	 when setting
	 collect cba into setcba
	 when setting
	 collect type into settype
	 finally (return (values (mapcar 'rest settype) setcba)))
    (let* ((setvbls (embedded-clfunc-args settype setcba marray))
	   (count
	    (apply
	     '+
	     (mapcar (lambda (inds) (abs (apply '* (rest inds)))) settype)))
	   (mvbvbls (loop repeat count collect (gensym "SETCB"))))
      (if (zerop count)
	  form
	  `(multiple-value-bind ,mvbvbls
	       ,form
	     (setf ,@(loop for mvbvbl in mvbvbls
			for setvbl in setvbls
			append (list setvbl mvbvbl))))))))

(defmacro defmcallback-old
    (name &optional (return-type :double) (argument-types :double)
     additional-argument-types marray function-variable)
  "Define a callback function used by GSL; the GSL function will call
   it with an additional `parameters' argument that is ignored.  the
   argument-types is a single type or list of types of the argument(s)
   that appear before parameters, and the additional-argument-types
   (default none) is a single type or list of types of the argument(s)
   that appear after parameters.  The argument types are C types or
   a list of a C type and a length, indicating a C array of that type
   for which each element will be passed as a separate argument.
   The return-type is the type that should be returned to GSL.
   If :success-failure, a GSL_SUCCESS code (0) is always returned;
   if :pointer, a null pointer is returned."
  (let* ((atl (if (listp argument-types) argument-types (list argument-types)))
	 (aatl (if (listp additional-argument-types) additional-argument-types
		   (list additional-argument-types)))
	 (cbargs (callback-args atl))
	 (cbaddl (callback-args aatl)))
    `(cffi:defcallback ,name
	 ,(if (eq return-type :success-failure) :int return-type)
	 (,@cbargs (params :pointer) ,@cbaddl)
       ;; Parameters as C argument are always ignored, because we have
       ;; CL specials to do the same job.
       (declare (ignore params) (special ,function-variable))
       ,(callback-set-mvb
	 `(funcall ,function-variable
		   ,@(append
		      (embedded-clfunc-args atl cbargs marray)
		      (embedded-clfunc-args aatl cbaddl marray)))
	 (append atl aatl)
	 (append cbargs cbaddl)
	 marray)
       ,@(case
	  return-type
	  (:success-failure
	   ;; We always return success, because if there was a
	   ;; problem, a CL error would be signalled.
	   '(success))
	  (:pointer
	   ;; For unclear reasons, some GSL functions want callbacks
	   ;; to return a void pointer which is apparently meaningless.
	   '((cffi:null-pointer)))))))

;;;;****************************************************************************
;;;; Definitions for making cbstruct
;;;;****************************************************************************

(defun set-cbstruct (cbstruct struct slots-values function-slotnames)
  "Make the slots in the foreign callback structure."
  (loop for (slot-name function) on function-slotnames by #'cddr
     do (set-slot-function cbstruct struct slot-name function))
  (set-parameters cbstruct struct)
  (when slots-values
    (loop for (slot-name value) on slots-values by #'cddr
       do (set-structure-slot cbstruct struct slot-name value))))

(defun make-cbstruct (struct slots-values &rest function-slotnames)
  "Make the callback structure."
  (let ((cbstruct (cffi:foreign-alloc struct)))
    (set-cbstruct cbstruct struct slots-values function-slotnames)
    cbstruct))

(defun make-cbstruct-object (object)
  "Make the callback structure based on the mobject definition."
  (apply
   'make-cbstruct
   (cbstruct-name object)
   (when (dimension-names object)
     (mapcan 'list (dimension-names object) (dimensions object)))
   (mapcan 'list (callback-labels object) (functions object))))

;;;;****************************************************************************
;;;; Classes that include callback information
;;;;****************************************************************************

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
   "A mobject that includes a callback function or functions, which
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

(defgeneric make-callbacks-fn (class args)
  (:documentation "Function to make forms that expand into defmcallback(s)."))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *callback-table* (make-hash-table :size 20)))

(defmacro def-make-callbacks (class arglist &body body)
  `(eval-when (:compile-toplevel :load-toplevel)
     (setf (gethash ',class *callback-table*)
	   (lambda ,arglist ,@body))))

(def-make-callbacks single-function (function)
  `(defmcallback ,function
       :double :double
       nil
       t
       ,function))

(export 'make-callbacks)
(defmacro make-callbacks (class &rest args)
  "Make the callbacks for the named class.  The args are generally of the form
     function [function...] dimension [dimension ...] scalars
   If 'scalars is T, the functions will be called with scalars, and should
   return answer as multiple values with #'values.  If it is NIL, it will
   be called with two arguments; it should read the values from the first
   array and write the answer in the second array."
  (apply (gethash class *callback-table*) args))

(defmethod print-object ((object callback-included) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'functions)
      (princ "for " stream)
      (princ (first (functions object)) stream)
      (princ "," stream))
    (princ "dimensions " stream)
    (princ (dimensions object) stream)))

;;;;****************************************************************************
;;;; TO BE OBSOLETE Using callback specification in function arugments
;;;;****************************************************************************

;;; to be obsolete?
(defun callback-arg-p (arglist &optional key)
  (member +callback-argument-name+ arglist :key key))

;;; to be obsolete?
(defun callback-replace-arg (replacement list)
  (subst replacement +callback-argument-name+ list))

;;; to be obsolete?
(defun callback-remove-arg (list &optional key)
  (remove +callback-argument-name+ list :key key))

;;;;****************************************************************************
;;;; Parsing :callback argument specification
;;;;****************************************************************************

;;; The :callbacks argument is a list of the form:
;;; (foreign-argument callback-structure-type function ...)
;;; where each function is of the form 
;;; (structure-slot-name
;;;   &optional (return-spec 'double-float) (argument-spec 'double-float)
;;;             set1-spec set2-spec marray)
;;; The :callback-dynamic is a list of functions corresponding in
;;; order to the cddr of the :callbacks argument (function... ) in which
;;; each element is a list of (function scalarsp dimensions...) where
;;; function = function designator, 
;;; scalarsp = flag determining whether to pass/accept scalars or arrays
;;; dimensions = dimension of the problem

(defparameter *callback-argument-components*
  '(foreign-argument callback-structure-type &rest functions))

(defparameter *callback-argument-function-components*
  '(structure-slot-name
   &optional
    (return-spec :double)
    (argument-spec :double)
    set1-spec set2-spec marray))

(defmacro parse-arglist (template arglist component)
  `(destructuring-bind ,template ,arglist
     (declare
      (ignorable
       ,@(remove-if
	  (lambda (sym) (member sym lambda-list-keywords))
	  (arglist-plain-and-categories template))))
     ,component))

(defmacro callback-argument-component (arglist component)
  `(parse-arglist ,*callback-argument-components* ,arglist ,component))

(defmacro callback-argument-function-component (arglist component)
  `(parse-arglist ,*callback-argument-function-components* ,arglist ,component))

(defun callback-argument-function-argspec (expr component)
  (case component
    (type (if (listp expr) (first expr) expr))
    (array-type (when (listp expr) (string-equal (second expr) :marray)))
    (dimensions (when (listp expr)
		  (let ((cddr (cddr expr)))
		    (if (rest cddr)
			(cons 'list cddr) 
			(first cddr)))))))

;;; (callback-argument-component '(foo bar baz (bof 1 2 2)) callback-structure-type)
;;; (callback-argument-function-component '(foo bar) argument-spec)

;;;;****************************************************************************
;;;; Form generation
;;;;****************************************************************************

(defun callback-symbol-set (callbacks symbols)
  "Generate the form to set each of the dynamic (special) variables
   to (function scalarsp dimensions...) in the body of the demfun for
   each of the callback functions."
  `((setf
     ,@(loop for symb in symbols
	  for list in callbacks
	  append (list symb (cons 'list list))))))

(defun callback-set-slots (callbacks symbols)
  `((set-cbstruct
     ,(callback-argument-component callbacks foreign-argument)
     ',(callback-argument-component callbacks callback-structure-type)
     nil 				; will have dimensions
     ,(cons
       'list
       (loop for symb in symbols
	  for fn
	  in (callback-argument-component callbacks functions)
	  append
	  `(',(callback-argument-function-component fn structure-slot-name)
	      ',symb))))))

(defun callback-args (types)
  "The arguments passed by GSL to the callback function."
  (mapcar (lambda (type)
	    (let ((symbol (gensym "ARG")))
	      (list symbol
		    (if (listp type)	; like (:double 3)
			:pointer	; C array
			type))))
	  (if (listp types) types (list types))))

;;;;****************************************************************************
;;;; Macro defmcallback NEW
;;;;****************************************************************************

(defmacro defmcallback (name dynamic-variable function-spec)
  (let* ((args (callback-args
		(list
		 (callback-argument-function-component function-spec argument-spec)
		 (callback-argument-function-component function-spec set1-spec)
		 (callback-argument-function-component function-spec set2-spec))))
	 (return-type 
	  (callback-argument-function-component function-spec return-spec)))
    `(cffi:defcallback ,name
	 ,(if (eq return-type :success-failure) :int return-type)
	 (,(first args)
	   (params :pointer)
	   ,@(list (second args) (third args)))
       ;; Parameters as C argument are always ignored, because we have
       ;; CL specials to do the same job.
       (declare (ignore params) (special ,dynamic-variable))
       (call-maybe-scalar
	(first ,dynamic-variable)
	,(cons 'list (mapcar 'first args))
	(second ,dynamic-variable)	; scalars
	(cddr ,dynamic-variable)	; dimensions for each argument
	(list		       ; which are marrays, which are cvectors
	 ,(callback-argument-function-argspec
	   (callback-argument-function-component function-spec argument-spec)
	   'array-type)
	 ,(callback-argument-function-argspec
	   (callback-argument-function-component function-spec set1-spec)
	   'array-type)
	 ,(callback-argument-function-argspec
	   (callback-argument-function-component function-spec set2-spec)
	   'array-type)))
       ,@(case
	  return-type
	  (:success-failure
	   ;; We always return success, because if there was a
	   ;; problem, a CL error would be signalled.
	   '(success))
	  (:pointer
	   ;; For unclear reasons, some GSL functions want callbacks
	   ;; to return a void pointer which is apparently meaningless.
	   '((cffi:null-pointer)))))))

(defun array-to-list (pointer length marray)
  (if marray
      (loop for i below length collect (maref pointer i))
      (loop for i below length collect (dcref pointer i)))))

(defun call-maybe-scalar (function arguments scalars dimensions marrays)
  "If scalars is a true, pass the function scalar arguments and set
   set1 and set2 to the scalar results."
  (if scalars
      (apply
       function (array-to-list (first arguments) (first dimensions) (first marrays)))
      (apply function arguments)))

