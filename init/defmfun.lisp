;; Macro for defining GSL functions.
;; Liam Healy 2008-04-16 20:49:50EDT defmfun.lisp
;; Time-stamp: <2009-01-07 22:02:49EST defmfun.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Macro defmfun
;;;;****************************************************************************

;;; Demfun is the main macro for defining functions (ordinary
;;; functions, generic functions, and methods) tthat call GSL
;;; functions.  It takes care of mapping the data from CL to C and
;;; then back out from C to CL.  Where the GSL function returns a
;;; condition code, it will insert a check that turns that result into
;;; a CL warning.  For generic functions and methods, it will generate
;;; the interfaces to all the specific GSL functions.

;;; Required arguments to defmfun:
;;; name        The name of the function being defined in CL
;;; arglist     The CL argument list for the function
;;; gsl-name    A string or list of strings representing the name(s) of
;;;             the GSL function(s)
;;; c-arguments A list of arguments like (symbol c-type)
;;;             (or list of such lists) to the GSL
;;;             functions, including CFFI declarations.
;;;             Anything not in arglist will be allocated.

;;; Keyword arguments to defmfun:
;;; c-return   A symbol naming a type, (e.g. :int, :double, :void),
;;;            or a list of (symbol type) to name the value,
;;;            or :error-code, :number-of-answers, :success-failure,
;;;            :true-false, :enumerate.  If :enumeration is given,
;;;            the :enumeration keyword argument will supply the name
;;;            of the enumeration.
;;; return     A list of quantities to return.
;;;            May be or include :c-return to include the c-return value
;;;            or its derivatives.
;;;            Default are allocated quantities in c-arguments, or :c-return if none.
;;; definition :function, :generic, :method, :methods
;;; qualifier  A method qualifier such as :after or :before.
;;; element-types
;;;            Permissible types for elements of arrays.  May be
;;;            NIL meaning all of *array-element-types*, :no-complex
;;;            meaning that list without the complex types, 
;;;	       :float meaning only the float types, :complex only
;;;	       the complex types, :float-complex both float and complex,
;;;            or a list of element types.
;;; index      Name under which this function should be cross-referenced; t
;;;            to use name, nil to not index.
;;; export     Whether to export the symbol.
;;; documentation
;;; inputs     Arrays whose values are used by the GSL function.
;;; outputs    Arrays that are written to in the GSL function.
;;; before     Forms to be evaluated before the foreign call.
;;; after      Forms to be evaluated after the foreign call.
;;; enumeration  The name of the enumeration return.
;;; gsl-version  The GSL version at which this function was introduced.

(defmacro defmfun (name arglist gsl-name c-arguments &rest key-args)
  "Definition of a GSL function."
  (expand-defmfun-wrap name arglist gsl-name c-arguments key-args))

;;; Utility for the helper functions
(defmacro with-defmfun-key-args (key-args &body body)
  "Bind defmfun's key arguments."
  `(destructuring-bind
    (&key (c-return :error-code)
     (return nil return-supplied-p)
     element-types
     (index t)
     (definition :function)
     (export (not (member definition (list :method :methods))))
     documentation inputs outputs before after enumeration qualifier
     gsl-version)
    ,key-args
    (declare (ignorable c-return return definition element-types
      index export documentation inputs outputs
      before after enumeration qualifier
      gsl-version indexed-functions)
     (special indexed-functions))
    ,@body))

(defparameter *defmfun-llk* '(&optional &key) "Possible lambda-list keywords.")

(defun optional-args-to-switch-gsl-functions (arglist gsl-name)
  "The presence/absence of optional arguments will switch between the
   first and second listed GSL function names."
  (and (intersection *defmfun-llk* arglist)
       (listp gsl-name)
       (or
	(every 'stringp gsl-name)
	(listp (first gsl-name)))))

(defun expand-defmfun-wrap (name arglist gsl-name c-arguments key-args)
  (let (indexed-functions)
    ;; workaround for compiler errors that don't see 'indexed-function is used
    (declare (ignorable indexed-functions))
    (with-defmfun-key-args key-args
      (setf indexed-functions (list))
      (wrap-index-export
       (cond
	 ((eq definition :generic)
	  (expand-defmfun-generic name arglist gsl-name c-arguments key-args))
	 ((eq definition :method)
	  (expand-defmfun-method name arglist gsl-name c-arguments key-args))
	 ((eq definition :methods)
	  (expand-defmfun-defmethods name arglist gsl-name c-arguments key-args))
	 ((optional-args-to-switch-gsl-functions arglist gsl-name)
	  (expand-defmfun-optional name arglist gsl-name c-arguments key-args))
	 ((eq definition :function)
	  (complete-definition 'cl:defun name arglist gsl-name c-arguments key-args)))
       name gsl-name key-args))))

(defun wrap-index-export (definition name gsl-name key-args)
  "Wrap the definition with index and export if requested.
   Use a progn if needed."
  (let ((index-export
	 (with-defmfun-key-args key-args
	   (if (eq index t)
	       (setf index name))
	   (flet ((mapnfn (gslnm) `(map-name ',index ,gslnm)))
	     (append
	      (when index
		(if indexed-functions
		    (mapcar #'mapnfn indexed-functions)
		    (if (listp gsl-name)
			(mapcar #'mapnfn gsl-name)
			(list (mapnfn gsl-name)))))
	      (when export `((export ',name))))))))
    (if index-export
	(if (symbolp (first definition))
	    `(progn ,definition ,@index-export)
	    `(progn ,@definition ,@index-export))
	(if (symbolp (first definition))
	    definition
	    `(progn ,definition ,@index-export)))))

;;;;****************************************************************************
;;;; A method for a generic function, on any class
;;;;****************************************************************************

(defun expand-defmfun-method (name arglist gsl-name c-arguments key-args)
  "Create a specific method for a previously-defined generic function."
  (with-defmfun-key-args key-args
    (if (listp gsl-name)
	(mapc (lambda (n) (push n indexed-functions)) gsl-name)
	(push gsl-name indexed-functions))
    (remf key-args :documentation)
      (complete-definition
       'cl:defmethod
       name
       arglist
       gsl-name
       c-arguments
       key-args
       (if (optional-args-to-switch-gsl-functions arglist gsl-name)
	   'body-optional-arg 'body-no-optional-arg)
       (listp gsl-name))))

;;;;****************************************************************************
;;;; Optional argument(s)
;;;;****************************************************************************

(defun expand-defmfun-optional
    (name arglist gsl-name c-arguments key-args)
  "Expand defmfun where there is an optional argument
   present, giving the choice between two different GSL functions."
  (complete-definition 'cl:defun name arglist gsl-name c-arguments key-args
		       'body-optional-arg))

(defun body-optional-arg
    (name arglist gsl-name c-arguments key-args)
  "Create the body of a defmfun with &optional in its arglist,
   where the presence or absence of the optional argument(s)
   selects one of two GSL functions."
  (let ((optpos (position '&optional arglist)))
    (if optpos
	(let ((mandatory-arglist (subseq arglist 0 optpos))
	      (optional-arglist (subseq arglist (1+ optpos))))
	  `(if ,(first optional-arglist)
	       ,(body-no-optional-arg 
		 name
		 (append mandatory-arglist optional-arglist)
		 (second gsl-name)
		 (second c-arguments)
		 key-args)
	       ,(body-no-optional-arg
		 name
		 mandatory-arglist
		 (first gsl-name)
		 (first c-arguments)
		 key-args))))))
