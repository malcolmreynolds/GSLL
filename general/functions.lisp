;********************************************************
; file:        functions.lisp                            
; description: Foreign callback functions.               
; date:        Sun Dec  9 2007 - 22:08                   
; author:      Liam Healy                                
; modified:    Sat Jan  5 2008 - 21:30
;********************************************************
;;; $Id: $

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
;;;; A function of a scalar double
;;;;****************************************************************************

;;; Used by numerical-integration, numerical-differentiation, chebyshev, ntuple.

(defmacro def-scalar-function
    (name
     &optional (return-type :double) (argument-type :double)
     (structure 'gsl-function)
     additional-slots)
  "Define the variable given by name
   as a foreign gsl-function that contains the callback
   of a CL function of the same name."
  (let ((argument (gensym "CB")))
    `(progn
      (cffi:defcallback ,name ,return-type
	  ((,argument ,argument-type) (params :pointer))
	(declare (ignore params))
	(,name ,argument))
      ,@(when
	 structure
	 ;; Assume that defcallback does not bind the variable 'name.
	 `((defparameter ,name (cffi:foreign-alloc ',structure))
	   (set-slot-function ,name ',structure 'function ',name)
	   (set-parameters ,name ',structure)
	   ,@(loop for slot in additional-slots
		   collect
		   `(set-structure-slot
		     ,name ',structure ',(first slot) ,(second slot))))))))

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

(defmacro with-c-doubles ((&rest cvector-names) &body body)
  "Provide named access to each element of a set of C arrays of doubles,
   for either reading or setting."
  (if (null (rest cvector-names))
      `(with-c-double ,(first cvector-names)
	,@body)
      `(with-c-double ,(first cvector-names)
	(with-c-doubles ,(rest cvector-names)
	  ,@body))))
