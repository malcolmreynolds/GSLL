;; Foreign callback functions.               
;; Liam Healy 
;; Time-stamp: <2009-02-07 10:38:20EST callback.lisp>
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

(export '(make-single-function undef-cbstruct))

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
;;;; Macros for defining a callback to wrap a CL function
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

(defun callback-args (types)
  "The arguments passed by GSL to the callback function."
  (mapcar (lambda (type)
	    (let ((symbol (gensym "ARG")))
	      (list symbol
		    (if (listp type)	; like (:double 3)
			:pointer	; C array
			type))))
	  (if (listp types) types (list types))))

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

(defmacro defmcallback
    (name &optional (return-type :double) (argument-types :double)
     additional-argument-types marray (function-name-or-lambda name))
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
       (declare (ignore params))
       ,(callback-set-mvb
	 `(,function-name-or-lambda
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

;;; To be deprecated
(defmacro defcbstruct
    (functions &optional (structure 'gsl-function) additional-slots)
  "Define a callback-related C struct used by GSL.
   This struct is bound to a CL special with the specified name.
   This macro can be used whenever a callback is defined and
   placed in a struct that has no other functions defined."
  (let ((name (make-symbol "CBSTRUCT"))
	(fnlist
	 ;; Make a list of (function slot-name ...) for each function.
	 (if (listp functions) functions (list `,functions 'function))))
    `(let ((,name (cffi:foreign-alloc ',structure)))
       ;; Create the C structure and bind CL variable to it.
       ;; Set all the function slots.
       ,@(loop for (fn slot-name) on fnlist by #'cddr collect
	      `(set-slot-function ,name ',structure ',slot-name ',fn))
       ;; Set the parameters.
       (set-parameters ,name ',structure)
       ;; Set any additional slots.
       ,@(loop for slot in additional-slots
	    collect
	    `(set-structure-slot
	      ,name ',structure ',(first slot) ,(second slot)))
       ,name)))

(defun make-cbstruct (struct slots-values &rest function-slotnames)
  (let ((cbstruct (cffi:foreign-alloc struct)))
    (loop for (slot-name function) on function-slotnames by #'cddr
       do (set-slot-function cbstruct struct slot-name function))
    (set-parameters cbstruct struct)
    (when slots-values
      (loop for (slot-name value) on slots-values by #'cddr
	 do (set-structure-slot cbstruct struct slot-name value)))
    cbstruct))

(defun undef-cbstruct (object)
  "Free foreign callback function.  It is not necessary to do this; think
   of the memory taken by an unused foreign function as much
   less than that used by an unused defun."
  (cffi:foreign-free object))

(defmacro with-computed-dimensions
    (dimensions-spec dimensions-used lambda-form &body body)
  (cl-utilities:once-only (lambda-form)
    `(let ((,dimensions-used
	    (if (and (listp ,lambda-form) (eq (first ,lambda-form) 'lambda))
		(length (second ,lambda-form))
		,dimensions-spec)))
       ,@body)))

(defun dimensions-from-lambda (lambda-form dimensions)
  "Determine the number of dimensions from the lambda form,
   or return the value specified in 'dimensions."
  (if (and (listp lambda-form) (eq (first lambda-form) 'lambda))
      (length (second lambda-form))
      dimensions))

(defmacro make-single-function
    (name
     &optional (return-type :double) (argument-type :double)
     (structure 'gsl-function)
     dimensions
     (dimensions-return dimensions)
     (marray t))
  "Define a callback and optionally a related C struct used by GSL.
   This struct is bound to a CL special with the specified name.
   This macro can be used whenever a callback is defined and
   placed in a struct that has no other functions defined."
  ;; I don't think structure=nil is ever used, but it is permissible
  (with-unique-names (cbsymb)
    (let ((dim (dimensions-from-lambda name dimensions)))
    `(progn
       (defmcallback ,cbsymb ,return-type
	 ,(if dim `((,argument-type ,dim)) argument-type)
	 ,(if dimensions-return `((:set ,argument-type ,dimensions-return)))
	 ,marray ,name)
       ,@(when
	  structure
	  `((defcbstruct ,cbsymb ,structure
	      ,(if dim `((dimensions ,dim))))))))))

(defmacro defun*
    (name args (dimensions-in . dimensions-out) &body body)
  "Define a function as with a defun, and make a callback for it so
  that it can be sent to GSL."
  (let ((dimin (if (listp dimensions-in) dimensions-in (list dimensions-in)))
	(dimout (if (listp dimensions-out) dimensions-out (list dimensions-out))))
    `(progn
       (defun ,name ,args ,@body)
       ,@(if dimensions-in
	     `((defmcallback ,name
		   :success-failure
		 ((:double ,@dimin))
		 ,(mapcar
		   (lambda (dims)
		     `(:set :double ,@(if (listp dims) dims (list dims))))
		   dimout)
		 t
		 ,name))
	     `((defmcallback ,name :success-failure :pointer :pointer))))))

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
   (functions
    :initarg :functions :reader functions
    :documentation "The names of the function(s) put into
     the callback.  These should correspond in order to
     callback-labels.")
   (dimensions :initarg :dimensions :reader dimensions))
  (:documentation
   "A mobject that includes a callback function or functions to GSL."))

(defmacro def-ci-subclass
    (class-name documentation cbstruct-name array-type callback-labels)
  `(defclass ,class-name (callback-included)
     ((cbstruct-name :initform ',cbstruct-name :allocation :class)
      (array-type :initform ',array-type :allocation :class)
      (callback-labels :initform ',callback-labels :allocation :class))
     (:documentation ,documentation)))
