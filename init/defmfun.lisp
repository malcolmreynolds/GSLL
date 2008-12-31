;; Macro for defining GSL functions.
;; Liam Healy 2008-04-16 20:49:50EDT defmfun.lisp
;; Time-stamp: <2008-12-31 12:29:44EST defmfun.lisp>
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
;;; after      Forms to be evaluated after the foreign call.
;;; enumeration  The name of the enumeration return.

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
     documentation inputs outputs after enumeration qualifier)
    ,key-args
    (declare (ignorable c-return return definition element-types
      index export
      documentation inputs outputs after enumeration qualifier)
     (special indexed-functions))
    ,@body))

(defparameter *defmfun-llk* '(&optional &key))

(defun llkp (arglist)
  "There is a lambda-list keyword present."
  (intersection *defmfun-llk* arglist))

(defun expand-defmfun-wrap (name arglist gsl-name c-arguments key-args)
  (let (indexed-functions)
    (declare (ignorable indexed-functions)) ; workaround for compiler errors that don't see it's used
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
	 ((and (llkp arglist) (listp gsl-name))
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
;;;; A defgeneric with methods, or the methods alone, for vectors or matrices
;;;;****************************************************************************

(defun expand-defmfun-generic (name arglist gsl-name c-arguments key-args)
  "Define a generic function and methods."
  ;; Need to scan the arglist for categories.
  ;; Can be mixed, unless it says 'both, in which case it can only be both.
  (with-defmfun-key-args key-args
    (multiple-value-bind (noclass-arglist categories)
	(arglist-plain-and-categories arglist)
      `(defgeneric ,name ,noclass-arglist
	(:documentation ,documentation)
	,@(if (member 'both categories)
	      (progn 
		(when (> (length categories) 1)
		  ;; Specify 'both alone
		  (error "Internal: mixed both and actual category."))
		;; Generate forms for matrix and vector
		(append
		 ;; Multiple C arguments for an &optional argument is
		 ;; accomodated with the llkp call.
		 (generate-methods
		  :method 'vector
		  name arglist gsl-name
		  (actual-array-c-type 'vector c-arguments (llkp arglist))
		  (copy-list key-args) 'vector)
		 (generate-methods
		  :method 'matrix
		  name arglist gsl-name
		  (actual-array-c-type 'matrix c-arguments (llkp arglist))
		  (copy-list key-args) 'matrix)))
	      ;; Generate forms for one category
	      (generate-methods
	       :method (first categories)
	       name arglist gsl-name c-arguments key-args))))))

(defun expand-defmfun-defmethods (name arglist gsl-name c-arguments key-args)
  "Define methods."
  ;; I presume 'both will not occur for pure methods
  (let ((categories (nth-value 1 (arglist-plain-and-categories arglist))))
    (generate-methods
     'cl:defmethod (first categories)
     name arglist gsl-name c-arguments key-args)))

(defun generate-methods
    (defn category name arglist gsl-name c-arguments key-args
     &optional replace-both)
  "Create all the methods for a generic function."
  ;; Methods may have &optional in two ways: the optional argument(s)
  ;; are defaulted if not supplied and a single GSL function called,
  ;; or the presence/absence of optional arguments switches between two
  ;; GSL functions.
  (with-defmfun-key-args key-args
    (mapcar (lambda (eltype)
	      (flet ((actual-gfn (gslname)
		       (let ((gsl-function-name
			      (actual-gsl-function-name
			       gslname category eltype)))
			 (push gsl-function-name indexed-functions)
			 gsl-function-name)))
		(remf key-args :documentation)
		(when (eq c-return :element-c-type)
		  (setf (getf key-args :c-return) (cl-cffi eltype)))
		(when (eq c-return :component-float-type)
		  (setf (getf key-args :c-return) (component-type eltype)))
		(if (and (llkp arglist) (listp (first gsl-name))) ; ad-hoc detection!
		    ;; The methods have optional argument(s) and
		    ;; multiple GSL functions for presence/absence of
		    ;; options
		    (complete-definition
		     defn
		     (if (eq defn :method) (list nil name) name)
		     (actual-class-arglist arglist eltype replace-both)
		     (mapcar #'actual-gfn gsl-name)
		     (mapcar (lambda (args)
			       (actual-element-c-type eltype args))
			     c-arguments)
		     key-args
		     'body-optional-arg)
		    (complete-definition
		     defn
		     (if (eq defn :method) (list nil name) name)
		     (actual-class-arglist arglist eltype replace-both)
		     (actual-gfn gsl-name)
		     (actual-element-c-type eltype c-arguments)
		     key-args))))
	    (element-types element-types))))

(defun actual-gsl-function-name (base-name category type)
  "Create the GSL or BLAS function name for data from the base name
   and the CL type."
  ;; (actual-gsl-function-name
  ;;  '("gsl_" :category :type "_swap") 'vector '(unsigned-byte 16))
  ;; "gsl_vector_ushort_swap"
  (if (listp base-name)
      (if (symbolp (first base-name))
	  ;; An explicit listing of types with function names
	  (getf base-name (cl-single type))
	  (let ((blas (search "blas" (first base-name) :test 'string-equal)))
	    (apply #'concatenate 'string
		   (substitute
		    (if (subtypep type 'complex) "_complex" "") :complex
		    (substitute
		     (if (subtypep type 'complex) "u" "") :suffix
		     (substitute
		      (cl-gsl type (not blas) blas) :type
		      (substitute
		       (if (subtypep type 'complex)
			   (cl-gsl (component-float-type type) nil blas)
			   "") :component-float-type
		       (substitute
			(string-downcase (symbol-name category)) :category
			base-name))))))))))

(defun number-class-from-type (type)
  "Find a class name that contains this type."
  ;; Not exhaustive; just trying to separate reals and complex.
  (cond ((subtypep type 'complex) 'complex)
	((subtypep type 'float) 'float)
	(t (error "Can't pass type ~a." type))))

(defun actual-class-arglist (arglist element-type &optional replace-both)
  "Replace the prototype arglist with an actual arglist."
  (loop for arg in arglist
     with replacing = t
     do
     (when (and replacing (member arg *defmfun-llk*))
       (setf replacing nil))
     collect
     (if (and replacing (listp arg))
	 (list (first arg)
	       (case (second arg)
		 (:element-type (number-class-from-type element-type))
		 (:component-float-type
		  (number-class-from-type (component-float-type element-type)))
		 (otherwise
		  (data-class-name
		   (if (and (eq (second arg) 'both) replace-both)
		       replace-both
		       (second arg))
		   element-type))))
	 (if (and (listp arg) (numberp (second arg)))
	     ;; optional arg default numerical value
	     (list (first arg)
		   (coerce (second arg) element-type))
	     arg))))

(defun arglist-plain-and-categories
    (arglist &optional (include-llk t))
  "Get arglist without classes and a list of categories."
  (loop for arg in arglist
     with getting-categories = t and categories
     do
     (when (and getting-categories (member arg *defmfun-llk*))
       (setf getting-categories nil))
     (when (and getting-categories (listp arg))
       ;; Collect categories (classes), but not default values to
       ;; optional arugments.
       (pushnew (second arg) categories))
     when (or (not (member arg *defmfun-llk*)) include-llk)
     collect
     (if (listp arg) (first arg) arg)
     into noclass-arglist
     finally (return (values noclass-arglist categories))))

(defun actual-array-c-type (category c-arguments &optional map-down)
  "Replace the declared proto-type with an actual GSL struct type
   in the GSL function name."
  (if map-down
      ;; If there are multiple C argument lists due to an optional
      ;; argument, than map this function onto each.
      (mapcar (lambda (carg) (actual-array-c-type category carg nil))
	      c-arguments)
      (mapcar
       (lambda (v)
	 (if (st-arrayp v)
	     (make-st (st-symbol v)
		      (intern
		       (apply #'concatenate
			      'string
			      (mapcar #'string
				      (substitute category :category (st-type v))))
		       :gsl))
	     v))
       c-arguments)))

(defun actual-element-c-type (element-type c-arguments)
  "Replace the generic element type :element-c-type with the
   actual element type."
  (mapcar 
   (lambda (v)
     (if (member (st-type v) '(:element-c-type :component-float-type))
	 (make-st (st-symbol v)
		  (if (eq (st-type v) :component-float-type)
		      (component-type element-type)
		      (cl-cffi element-type)))
	 v))
   c-arguments))

;;; (actual-class-arglist '((v1 vector) (v2 vector) (m matrix) x) '(unsigned-byte 16))
;;; ((V1 VECTOR-UNSIGNED-BYTE-16) (V2 VECTOR-UNSIGNED-BYTE-16)
;;;  (M MATRIX-UNSIGNED-BYTE-16) X)
;;; (V1 V2 M X)
;;; (MATRIX VECTOR)

;;;;****************************************************************************
;;;; A method for a generic function, on any class
;;;;****************************************************************************

(defun expand-defmfun-method (name arglist gsl-name c-arguments key-args)
  "Create a specific method for a previously-defined generic function."
  (with-defmfun-key-args key-args
    (if (listp gsl-name)
	(mapc (lambda (n) (push n indexed-functions)) gsl-name)
	(push gsl-name indexed-functions))
    (with-defmfun-key-args key-args
      (remf key-args :documentation)
      (complete-definition
       'cl:defmethod
       name
       arglist
       gsl-name
       c-arguments
       key-args
       (if (and (llkp arglist) (listp gsl-name))
	   ;; Even though it might have optional arguments, if there
	   ;; is only one GSL function, we use 'body-no-optional-arg.
	   'body-optional-arg 'body-no-optional-arg)
       (listp gsl-name)))))

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

;;;;****************************************************************************
;;;; A single function
;;;;****************************************************************************

(defun defgeneric-method-p (name)
  "When defining :method in a defgeneric, (nil foo) is used for
   the name, and foo will be returned from this function."
  (if (and (listp name) (null (first name)))
      (second name)))

(defun complex-scalars (cl-arguments c-arguments-types)
  (loop for sd in c-arguments-types
	for cl-type = (cffi-cl (st-type sd))
	append
	(when (and cl-type (subtypep cl-type 'complex)
		   (member (st-symbol sd)
			   (arglist-plain-and-categories cl-arguments)))
	  (list (list (st-symbol sd)
		      (gensym (string (st-symbol sd)))
		      (st-type sd))))))

(defun stupid-code-walk-find-variables (sexp)
  "This will work with the simplest s-expression forms only to find
   variables used."
  (labels ((scwfv (sexp)
	     (if (atom sexp)
		 (when (and (symbolp sexp)
			    (not (eql (symbol-package sexp)
				      (find-package :keyword))))
		   (list sexp))
		 (unless (member (first sexp) '(quote))
		   (mapcan #'scwfv (rest sexp))))))
    (remove nil (remove-duplicates (scwfv sexp)))))

(defun variables-used-in-c-arguments (c-arguments)
  "Find the arguments passed to the C function.  This is a poor
  quality code walker, but is sufficient for actual usage of defmfun."
  (remove-duplicates
   (mapcan (lambda (carg)
	     (stupid-code-walk-find-variables (st-symbol carg)))
	   c-arguments)
   :from-end t))

#+native
(defun native-pointer (array-symbols body)
  "Wrap the body with a form that obtains the native pointer
   and protects it during execution of the body."
  (if array-symbols
      ;; http://www.sbcl.org/manual/Calling-Lisp-From-C.html
      (native-pointer-protect array-symbols body)
      body))

(defun complete-definition
    (definition name arglist gsl-name c-arguments key-args
     &optional
     (body-maker 'body-no-optional-arg)
     (mapdown (eq body-maker 'body-optional-arg)))
  "A complete definition form, starting with defun, :method, or defmethod."
  (destructuring-bind
	(&key documentation inputs outputs after qualifier &allow-other-keys) key-args
    (declare (ignorable inputs outputs)) ; workaround for compiler errors that don't see it's used
    `(,definition
	 ,@(when (and name (not (defgeneric-method-p name)))
		 (list name))
	 ,@(when qualifier (list qualifier))
       ,arglist
       ,(declaration-form
	 (cl-argument-types arglist c-arguments)
	 (set-difference (arglist-plain-and-categories arglist nil)
			 (union
			  (if mapdown
			      (apply 'union
				     (mapcar 'variables-used-in-c-arguments c-arguments))
			      (variables-used-in-c-arguments c-arguments))
			  ;; Forms in :after are checked for used variables
			  (stupid-code-walk-find-variables (cons 'values after)))))
       ,@(when documentation (list documentation))
       #-native
       ,(funcall body-maker name arglist gsl-name c-arguments key-args)
       #+native
       ,(native-pointer
	 (union inputs outputs)
	 (funcall body-maker name arglist gsl-name c-arguments key-args)))))

(defun body-no-optional-arg (name arglist gsl-name c-arguments key-args)
  "Expand the body (computational part) of the defmfun."
  (with-defmfun-key-args key-args
    (let* ((cret-type (if (member c-return *special-c-return*)
			  :int
			  (if (listp c-return) (st-type c-return) c-return)))
	   (cret-name
	    (if (listp c-return) (st-symbol c-return) (make-symbol "CRETURN")))
	   (complex-args (complex-scalars arglist c-arguments))
	   (allocated		     ; Foreign objects to be allocated
	    (remove-if
	     (lambda (s)
	       (member s (arglist-plain-and-categories arglist nil)))
	     (variables-used-in-c-arguments c-arguments)))
	   (allocated-decl
	    (append
	     (mapcar
	      (lambda (s) (find s c-arguments :key #'st-symbol))
	      allocated)))
	   (clret (or			; better as a symbol macro
		   (substitute cret-name :c-return return)
		   (mapcan #'cl-convert-form allocated-decl)
		   outputs
		   (unless (eq c-return :void)
		     (list cret-name)))))
      `(,@(if (or allocated-decl complex-args)
	      `(cffi:with-foreign-objects
		   ,(mapcar #'wfo-declare
			    (append allocated-decl 
				    (mapcar #'rest complex-args))))
	      '(let ()))
	  #-native ,@(mapcar (lambda (v) `(copy-cl-to-c ,v)) inputs)
	  (let ((,cret-name
		 (cffi:foreign-funcall
		  ,gsl-name
		  ,@(mapcan
		     (lambda (arg)
		       (let ((cfind	; variable is complex
			      (first (member (st-symbol arg)
					     complex-args :key 'first))))
			 (if cfind ; so substitute call to complex-to-gsl
			     `(,(third cfind)
				(complex-to-gsl ,(first cfind) ,(second cfind)))
			     ;; otherwise use without conversion
			     (list (if (member (st-symbol arg) allocated)
				       :pointer
				       (st-type arg))
				   (st-symbol arg)))))
		     c-arguments)
		  ,cret-type)))
	    ,@(case c-return
		    (:void `((declare (ignore ,cret-name))))
		    (:error-code	; fill in arguments
		     `((check-gsl-status ,cret-name
					 ',(or (defgeneric-method-p name) name)))))
	    #-native
	    ,@(when outputs
		    (mapcar
		     (lambda (x) `(setf (cl-invalid ,x) t (c-invalid ,x) nil))
		     outputs))
	    ,@(when (eq cret-type :pointer)
		    `((check-null-pointer
		       ,cret-name
		       ,@'('memory-allocation-failure "No memory allocated."))))
	    ,@after
	    (values
	     ,@(defmfun-return
		c-return cret-name clret allocated return return-supplied-p
		enumeration outputs)))))))

(defun defmfun-return
    (c-return cret-name clret allocated return return-supplied-p enumeration outputs)
  "Generate the return computation expression for defmfun."
  (case c-return
    (:number-of-answers
     (mapcan
      (lambda (vbl seq)
	`((when (> ,cret-name ,seq) ,vbl)))
      clret
      (loop for i below (length clret) collect i)))
    (:success-failure
     (if (equal clret outputs)
	 ;; success-failure more important than passed-in
	 `((success-failure ,cret-name))
	 (remove cret-name		; don't return c-return itself
		 `(,@clret (success-failure ,cret-name)))))
    (:success-continue
     (if (equal clret outputs)
	 ;; success-failure more important than passed-in
	 `((success-continue ,cret-name))
	 (remove cret-name		; don't return c-return itself
		 `(,@clret (success-continue ,cret-name)))))
    (:true-false
     `((not (zerop ,cret-name))))
    (:enumerate
     `((cffi:foreign-enum-keyword ',enumeration ,cret-name)))
    (t (unless
	   (or
	    (and (eq c-return :error-code)
		 (not outputs)
		 (not allocated)
		 (not return-supplied-p))
	    (and (null return) return-supplied-p))
	 clret))))
