;; Foreign callback functions.               
;; Liam Healy 
;; Time-stamp: <2009-03-28 20:59:14EDT callback.lisp>
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
;;;; Definitions for making cbstruct
;;;;****************************************************************************

(defun set-cbstruct (cbstruct structure-name slots-values function-slotnames)
  "Make the slots in the foreign callback structure."
  (loop for (slot-name function) on function-slotnames by #'cddr
     do (set-slot-function cbstruct structure-name slot-name function))
  (set-parameters cbstruct structure-name)
  (when slots-values
    (loop for (slot-name value) on slots-values by #'cddr
       do (set-structure-slot cbstruct structure-name slot-name value))))

(defun make-cbstruct (struct slots-values &rest function-slotnames)
  "Make the callback structure."
  (assert struct (struct) "Structure must be supplied.")
  (let ((cbstruct (cffi:foreign-alloc struct)))
    (set-cbstruct cbstruct struct slots-values function-slotnames)
    cbstruct))

;;;;****************************************************************************
;;;; to be obsolete make-callbacks
;;;;****************************************************************************

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
;;;; Parsing :callback argument specification
;;;;****************************************************************************

;;; The :callbacks argument is a list of the form:
;;; (foreign-argument callback-structure-type dimension-names function ...)
;;; where each function is of the form 
;;; (structure-slot-name
;;;   &optional (return-spec 'double-float) (argument-spec 'double-float)
;;;             set1-spec set2-spec)
;;; and each of the *-spec are (type array-type &rest dimensions).
;;; The :callback-dynamic is a list of functions corresponding in
;;; order to the cddr of the :callbacks argument (function... ) in which
;;; each element is a list of (function scalarsp &rest dimensions)
;;; where
;;; function = function designator, 
;;; scalarsp = flag determining whether to pass/accept scalars or arrays
;;; dimensions = dimension of the problem

(defun parse-callback-static (callbacks component)
  "Get the information component from the callbacks list."
  (case component
    (foreign-argument (first callbacks))
    (callback-structure-type (second callbacks))
    (dimension-names (third callbacks))
    (functions (nthcdr 3 callbacks))))

(defun number-of-callbacks (callbacks)
  (length (parse-callback-static callbacks 'functions)))

(defun parse-callback-fnspec (fnspec component)
  "From the :callbacks argument, parse a single function specification."
  (ecase component
    (structure-slot-name (first fnspec))
    (return-spec (second fnspec))
    (arguments-spec (cddr fnspec))))

(defun parse-callback-argspec (argspec component)
  "From the :callbacks argument, parse a single argument of a single
  function specification."
  (ecase component
    (io (first argspec))		; :input or :output
    (element-type (second argspec))	; :double
    (array-type (third argspec))	; :marray or :cvector
    (dimensions (nthcdr 3 argspec))))

;;;;****************************************************************************
;;;; Using callback specification in function arugments
;;;;****************************************************************************

(defun callback-arg-p (arglist callbacks &optional key)
  (member (parse-callback-static callbacks 'foreign-argument) arglist :key key))

(defun callback-replace-arg (replacement list callbacks)
  "Replace in the list the symbol representing the foreign callback argument."
  (if callbacks
      (subst
       replacement
       (parse-callback-static callbacks 'foreign-argument)
       list)
      list))

(defun callback-remove-arg (list callbacks &optional key)
  "Remove from the list the symbol representing the foreign callback argument."
  (remove (parse-callback-static callbacks 'foreign-argument) list :key key))

;;;;****************************************************************************
;;;; Form generation
;;;;****************************************************************************

(defun callback-dynamic-lister (callback-dynamic)
  (mapcar (lambda (l) (cons 'list l)) callback-dynamic))

(defun callback-symbol-set (callbacks symbols)
  "Generate the form to set each of the dynamic (special) variables
   to (function scalarsp dimensions...) in the body of the demfun for
   each of the callback functions."
  (when callbacks
    `((setf
       ,@(loop for symb in symbols
	    for list in (callback-dynamic-lister callbacks)
	    append (list symb list))))))

(defun callback-set-slots (callbacks dynamic-variables)
  (when callbacks
    `((set-cbstruct
       ,(parse-callback-static callbacks 'foreign-argument)
       ',(parse-callback-static callbacks 'callback-structure-type)
       (mapcan 'list
	       ',(parse-callback-static callbacks 'dimension-names)
	       (cddr ,(caar dynamic-variables)))
       ,(cons
	 'list
	 (loop for symb in (second dynamic-variables)
	    for fn in (parse-callback-static callbacks 'functions)
	    append
	    `(',(parse-callback-fnspec fn 'structure-slot-name)
		',symb)))))))

(defun callback-args (argspec)
  "The arguments passed by GSL to the callback function."
  (mapcar (lambda (arg)
	    (if (listp arg)
		(let ((symbol (gensym "ARG")))
		  (list symbol
			(if (parse-callback-argspec arg 'array-type)
			    :pointer	; C array
			    (parse-callback-argspec arg 'element-type))))
		arg))
	  argspec))

;;;;****************************************************************************
;;;; Macro defmcallback
;;;;****************************************************************************

(defun make-defmcallbacks (callbacks callback-names function-names)
  (when callbacks
    (mapcar
     (lambda (cb vbl fspec) `(defmcallback ,cb ,vbl ,fspec))
     callback-names function-names
     (parse-callback-static callbacks 'functions))))

(defmacro defmcallback (name dynamic-variable function-spec)
  (let* ((argspec (parse-callback-fnspec function-spec 'arguments-spec))
	 (return-type (parse-callback-fnspec function-spec 'return-spec))
	 (args (callback-args argspec))
	 (slug (make-symbol "SLUG")))
    `(cffi:defcallback ,name
	 ,(if (eq return-type :success-failure) :int return-type)
	 (,@(substitute `(,slug :pointer) :slug args))
       ;; Parameters as C argument are always ignored, because we have
       ;; CL specials to do the same job.
       (declare (ignore ,slug) (special ,dynamic-variable))
       (funcall ,dynamic-variable ,@(mapcar 'st-symbol (remove :slug args))))))
