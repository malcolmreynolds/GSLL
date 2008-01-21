;; Foreign callback functions.               
;; Liam Healy 
;; Time-stamp: <2008-01-20 18:12:42EST functions.lisp>
;; $Id: $

(in-package :gsl)

;;; Callback functions are functions which are passed as data; to Lisp
;;; that means they are just functions, but C makes a distinction.
;;; They are needed by several GSL tasks.
;;; Functions that take one double-float and return one double-float
;;; and are defined using the gsl_function structure (see gsl_math.h)
;;; are called scalar functions.  They are used in 
;;; numerical-integration, numerical-differentiation, chebyshev and
;;; definitions to aid in creating and using them are provided here.
;;; The idea behind the definitions is that the boundary between the CL
;;; and C functions is a narrow as possible; #'defun-scalar is
;;; macro that allows one to define a function in Lisp and make
;;; use it in these GSL tasks.

;;; Other GSL tasks make use of callback functions with different
;;; characteristics.  Since they are specific to each of the tasks,
;;; they are defined with those tasks.  A complexity encountered with
;;; using a vector of double floats not present with scalars is that
;;; there is no portable way to make a C array available directly to
;;; Lisp, so callbacks using C arrays must read them using a macro.
;;; Therefore it is necessary to define the function in a way that
;;; prevents its use in Lisp; to ameliorate this, the macro
;;; #'with-c-double is provided to give named access to the elements.

(export
 '(def-scalar-function undef-scalar-function defun-scalar
   with-c-double with-c-doubles))

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
;;;; Macros for defining a callback and placing in a structure
;;;;****************************************************************************

;;; Usage example for scalar function (e.g. numerical-integration,
;;; numerical-differentiation, chebyshev, ntuple).  
;;; (defmcallback myfn :double :double)
;;; Usage example for gsl-vector function (e.g. roots-multi)
;;; (defmcallback myfn :pointer :int (:pointer))
;;; Usage example for function and derivative
;;; (defmcallback fdf :pointer :double (:pointer :pointer))
;;; (defmcallback fdf :success-failure :int (:pointer :pointer))

(defmacro defmcallback
    (name &optional (return-type :double) (argument-types :double)
     additional-argument-types)
  "Define a callback function used by GSL; the GSL function will call
   it with an additional `parameters' argument that is ignored.  the
   argument-types is a single type or list of types of the argument(s)
   that appear before parameters, and the additional-argument-types
   (default none) is a single type or list of types of the argument(s)
   that appear after parameters.  The return-type is the type that
   should be returned to GSL.  If :success-failure, a GSL_SUCCESS
   code (0) is always returned; if :pointer, a null pointer is
   returned."
  (flet ((arg-type (types)
	   (when types
	     (mapcar (lambda (type) (list (gensym "ARG") type))
		     (if (listp types) types (list types))))))
    (let ((arguments (arg-type argument-types))
	  (additional-arguments (arg-type additional-argument-types)))
      `(cffi:defcallback ,name
	,(if (eq return-type :success-failure) :int return-type)
	(,@arguments (params :pointer) ,@additional-arguments)
	;; Parameters as C argument are always ignored, because we have
	;; CL specials to do the same job.
	(declare (ignore params))
	(,name ,@(mapcar #'first (append arguments additional-arguments)))
	,@(case
	   return-type
	   (:success-failure
	    ;; We always return success, because if there was a
	    ;; problem, a CL error would be signalled.
	    '(success))
	   (:pointer
	    ;; For unclear reasons, some GSL functions want callbacks
	    ;; to return a void pointer which is apparently meaningless.
	    '((cffi:null-pointer))))))))

(defmacro defcbstruct
    (functions &optional (structure 'gsl-function) additional-slots)
  "Define a callback-related C struct used by GSL.
   This struct is bound to a CL special with the specified name.
   This macro can be used whenever a callback is defined and
   placed in a struct that has no other functions defined."
  (let ((name
	 ;; Bind a CL special under this name to the C structure.
	 (if (listp functions) (first functions) functions))
	(fnlist
	 ;; Make a list of (function slot-name ...) for each function.
	 (if (listp functions) functions (list `,functions 'function))))
    `(progn
      ;; Create the C structure and bind CL variable to it.
      (defparameter ,name (cffi:foreign-alloc ',structure))
      ;; Set all the function slots.
      ,@(loop for (fn slot-name) on fnlist by #'cddr collect
	      `(set-slot-function ,name ',structure ',slot-name ',fn))
      ;; Set the parameters.
      (set-parameters ,name ',structure)
      ;; Set any additional slots.
      ,@(loop for slot in additional-slots
	      collect
	      `(set-structure-slot
		,name ',structure ',(first slot) ,(second slot))))))

(defmacro def-scalar-function
    (name
     &optional (return-type :double) (argument-type :double)
     (structure 'gsl-function)
     additional-slots
     additional-argument-types)
  "Define a callback and optionally a related C struct used by GSL.
   This struct is bound to a CL special with the specified name.
   This macro can be used whenever a callback is defined and
   placed in a struct that has no other functions defined."
  `(progn
    (defmcallback ,name ,return-type ,argument-type ,additional-argument-types)
    ,@(when
       structure
       `((defcbstruct ,name ,structure ,additional-slots)))))

(defun undef-scalar-function (name)
  "Free foreign callback function.  It is not necessary to do this; think
   of the memory taken by an unused foreign function as much
   less than that used by an unused defun."
  (cffi:foreign-free (symbol-value name))
  (makunbound name))

;;; Combine a defun and def-scalar-function in one:
(defmacro defun-scalar (name arglist &body body)
  "Define a function of a scalar double-float argument returning
   a double-float in CL and C."
  `(progn
    (defun ,name ,arglist ,@body)
    (def-scalar-function ,name)))

;;;;****************************************************************************
;;;; Vector of doubles
;;;;****************************************************************************

;;; Unfortunately, because vectors must be copied between languages
;;; (even with vector-sap in callbacks, unless vector-sap can be
;;; setfed), there is no way to provide a function of a vector and
;;; have it work in both languages.  As a consolation the macro
;;; #'with-c-double is provided to make things easier.

(defmacro with-c-double
    ((c-vector &rest element-names) &body body)
  "Provide named access to each element of a C array of doubles, for either
   reading or setting."
  `(symbol-macrolet
    ,(loop for i from 0 for a in element-names
	   collect `(,a (double-to-cl ,c-vector ,i)))
    ,@body))

(defmacro with-c-doubles
    ((&rest cvector-names) &body body)
  "Provide named access to each element of a C array of doubles, for either
   reading or setting."
  `(symbol-macrolet
    ,(loop for (c-vector . element-names) in cvector-names
	   append
	   (loop for i from 0 for a in element-names
		 collect `(,a (double-to-cl ,c-vector ,i))))
    ,@body))
