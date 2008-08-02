;; Macro for defining GSL functions.
;; Liam Healy 2008-04-16 20:49:50EDT defmfun.lisp
;; Time-stamp: <2008-08-02 19:18:11EDT defmfun.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Macro defmfun
;;;;****************************************************************************

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
;;; null-pointer-info
;;;            Return value if C function returns a null pointer.
;;; documentation
;;; inputs     Arrays whose values are used by the GSL function
;;; outputs    Arrays that are written to in the GSL function
;;; after        After method.
;;; enumeration  The name of the enumeration return.
;;; global     Bind variable(s) in a let* enclosing the whole body.

(defmacro defmfun (name arglist gsl-name c-arguments &rest key-args)
  "Definition of a GSL function."
  (expand-defmfun-wrap name arglist gsl-name c-arguments key-args))

;;; Utility for the helper functions
(defmacro with-defmfun-key-args (key-args &body body)
  "Bind defmfun's key arguments."
  `(destructuring-bind
    (&key (c-return :error-code)
     (return nil return-supplied-p)
     (definition :function)		; :function, :generic, :method, :methods
     element-types
     (index t)
     (export (not (member definition (list :method :methods))))
     null-pointer-info documentation inputs outputs after enumeration
     global)
    ,key-args
    (declare (ignorable c-return return definition element-types
      index export
      null-pointer-info documentation inputs outputs after enumeration
      global)
     (special defn indexed-functions))
    ,@body))

(defun expand-defmfun-wrap (name arglist gsl-name c-arguments key-args)
  (let (defn indexed-functions)
    (declare (ignore defn))
    (with-defmfun-key-args key-args
      (setf indexed-functions (list))
      (wrap-index-export
       (cond
	 ((and (member '&optional arglist) (listp gsl-name))
	  (expand-defmfun-optional name arglist gsl-name c-arguments key-args))
	 ((eq definition :function)
	  (setf defn 'cl:defun)
	  (expand-defmfun-plain name arglist gsl-name c-arguments key-args))
	 ((eq definition :generic)
	  (expand-defmfun-generic name arglist gsl-name c-arguments key-args))
	 ((eq definition :method)
	  (expand-defmfun-method name arglist gsl-name c-arguments key-args))
	 ((eq definition :methods)
	  (expand-defmfun-defmethods name arglist gsl-name c-arguments key-args))
	 (t
	  (expand-defmfun-plain name arglist gsl-name c-arguments key-args)))
       name gsl-name key-args))))

(defun wrap-index-export (definition name gsl-name key-args)
  "Wrap the definition with index and export if requested.
   Use a progn if needed."
  (let ((index-export
	 (with-defmfun-key-args key-args
	   (if (eq index t)
	       (setf index name))
	   (flet ((mapnfn (gsl-name) `(map-name ',index ,gsl-name)))
	     (append
	      (when index
		(if indexed-functions
		    (mapcar #'mapnfn indexed-functions)
		    (list (mapnfn gsl-name))))
	      (when export `((export ',name))))))))
    (if index-export
	(if (symbolp (first definition))
	    `(progn ,definition ,@index-export)
	    `(progn ,@definition ,@index-export))
	(if (symbolp (first definition))
	    definition
	    `(progn ,definition ,@index-export)))))

;;;;****************************************************************************
;;;; Test cases
;;;;****************************************************************************

;;; Simple function
#+(or)
(expand-defmfun-wrap
 'airy-Ai '(x)
 "gsl_sf_airy_Ai_e"
 '((x :double) :mode (ret sf-result))
 '(:documentation "The Airy function Ai(x)."))

;;; Generic function and methods of a vector
#+(or)
(expand-defmfun-wrap
 'gsl-zerop
 '((v vector))
 '("gsl_vector" :type "_isnull")
 '(((pointer v) gsl-vector-c))
 '(:c-return :boolean
   :definition :generic
   :documentation			; FDL
   "All elements of vector v are zero."))

;;; Generic function and methods of a matrix
#+(or)
(expand-defmfun-wrap
 'gsl-zerop
 '((v matrix))
 '("gsl_" :category :type "_isnull")
 '(((pointer v) gsl-matrix-c))
 '(:c-return :boolean
   :definition :generic
   :documentation			; FDL
   "All elements of v are zero."))

;;; Defgeneric over both matrices and vectors
#+(or)
(expand-defmfun-wrap
 'gsl-zerop
 '((v both))
 '("gsl_" :category :type "_isnull")
 '(((pointer v) (gsl- :category -c)))
 '(:c-return :boolean
   :definition :generic
   :documentation			; FDL
   "All elements of v are zero."))

;;; Methods without generic function declaration
#+(or)
(expand-defmfun-wrap
 'gsl-zerop
 '((v vector))
 '("gsl_vector" :type "_isnull")
 '(((pointer v) gsl-vector-c))
 '(:c-return :boolean
   :definition :methods
   :documentation			; FDL
   "All elements of vector v are zero."))

;;; Regular function with optional arguments
#+(or)
(expand-defmfun-wrap
 'evaluate-chebyshev
 '(chebyshev x &optional order)
 '("gsl_cheb_eval_err" "gsl_cheb_eval_n_err")
 '(((chebyshev :pointer) (x :double) (result :double) (abserr :double))
   ((chebyshev :pointer) (order size)
    (x :double) (result :double) (abserr :double)))
 '(:documentation			; FDL
   "Evaluate the Chebyshev series at a point x, returning result and
   an estimate of its absolute error."))

;;;;****************************************************************************
;;;; A defgeneric with methods, or the methods alone, for vectors or matrices
;;;;****************************************************************************

(defun expand-defmfun-generic (name arglist gsl-name c-arguments key-args)
  "Define a generic function and methods."
  ;; Need to scan the arglist for categories.
  ;; Can be mixed, unless it says 'both, in which case it can only be both.
  (with-defmfun-key-args key-args
    ;;(setf defn :methods)
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
		 (generate-methods
		  :method 'vector
		  name arglist gsl-name (actual-array-c-type 'vector c-arguments)
		  key-args 'vector)
		 (generate-methods
		  :method 'matrix
		  name arglist gsl-name (actual-array-c-type 'matrix c-arguments)
		  key-args 'matrix)))
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
    (new-definition category name arglist gsl-name c-arguments key-args
     &optional replace-both)
  "Create all the methods for a generic function."
  (with-defmfun-key-args key-args
    (setf defn new-definition)
    (mapcar (lambda (eltype)
	      (remf key-args :documentation)
	      (when (eq c-return :element-c-type)
		(setf (getf key-args :c-return) (cl-ffa eltype)))
	      (when (eq c-return :component-float-type)
		(setf (getf key-args :c-return) (cl-ffa (component-type eltype))))
	      (expand-defmfun-plain
	       (if (eq new-definition :method) (list nil name) name)
	       (actual-class-arglist arglist eltype replace-both)
	       (let ((gsl-function-name
		      (actual-gsl-function-name
		       gsl-name category eltype)))
		 (push gsl-function-name indexed-functions)
		 gsl-function-name)
	       (actual-element-c-type eltype c-arguments)
	       key-args))
	    (case element-types
	      ((nil t) *array-element-types*)
	      (:no-complex *array-element-types-no-complex*)
	      (:float *float-types*)
	      (:complex *complex-types*)
	      (:float-complex *float-complex-types*)
	      (t element-types)))))

(defun actual-gsl-function-name (base-name category type)
  "Create the GSL or BLAS function name for data from the base name
   and the CL type."
  ;; (actual-gsl-function-name
  ;;  '("gsl_" :category :type "_swap") 'vector '(unsigned-byte 16))
  ;; "gsl_vector_ushort_swap"
  (let ((blas (search "blas" (first base-name) :test 'string-equal)))
    (if (listp base-name)
	(apply #'concatenate 'string
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
		   base-name))))))))

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
     (when (and replacing (member arg '(&optional)))
       (setf replacing nil))
     collect
     (if (and replacing (listp arg))
	 (list (first arg)
	       (case (second arg)
		 (:element-c-type (number-class-from-type element-type))
		 (:component-float-type
		  (number-class-from-type (component-float-type element-type)))
		 (otherwise
		  (data-class-name
		   (if (and (eq (second arg) 'both) replace-both)
		       replace-both
		       (second arg))
		   element-type))))
	 arg)))

(defun arglist-plain-and-categories (arglist)
  "Get arglist without classes and a list of categories."
  (loop for arg in arglist
	with replacing = t and categories
	do
	(when (and replacing (member arg '(&optional)))
	  (setf replacing nil))
	(when (and replacing (listp arg))
	  (pushnew (second arg) categories))
	collect
	(if (and replacing (listp arg)) (first arg) arg)
	into noclass-arglist
	finally (return (values noclass-arglist categories))))

(defun actual-array-c-type (category c-arguments)
  "Replace the declared proto-type with an actual GSL struct type."
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
   c-arguments))

(defun actual-element-c-type (element-type c-arguments)
  "Replace the generic element type :element-c-type with the
   actual element type."
  (mapcar 
   (lambda (v)
     (if (member (st-type v) '(:element-c-type :component-float-type))
	 (make-st (st-symbol v)
		  (if (eq (st-type v) :component-float-type)
		      (component-type element-type)
		      (cl-ffa element-type)))
	 v))
   c-arguments))

;;; (actual-class-arglist '((v1 vector) (v2 vector) (m matrix) x) '(unsigned-byte 16))
;;; ((V1 VECTOR-UNSIGNED-BYTE-16) (V2 VECTOR-UNSIGNED-BYTE-16)
;;;  (M MATRIX-UNSIGNED-BYTE-16) X)
;;; (V1 V2 M X)
;;; (MATRIX VECTOR)

;;;;****************************************************************************
;;;; A method for a generic function
;;;;****************************************************************************

(defun expand-defmfun-method (name arglist gsl-name c-arguments key-args)
  "Create a specific method for a previously-defined generic function."
  (with-defmfun-key-args key-args
    (remf key-args :documentation)
    (setf defn 'cl:defmethod)
    (expand-defmfun-plain
     name
     arglist
     (progn
       (push gsl-name indexed-functions)
       gsl-name)
     c-arguments
     key-args)))

;;;;****************************************************************************
;;;; Optional argument(s)
;;;;****************************************************************************

(defun expand-defmfun-optional
    (name arglist gsl-name c-arguments key-args)
  "Expand defmfun where there is an optional argument
   present, giving the choice between two different GSL functions."
  (with-defmfun-key-args key-args
    (let ((optpos (position '&optional arglist)))
      (if optpos
	  (let ((mandatory-arglist (subseq arglist 0 optpos))
		(optional-arglist (subseq arglist (1+ optpos))))
	    (remf key-args :documentation)
	    (setf indexed-functions gsl-name)
	    `((defun ,name ,arglist
		,documentation
		(if ,(first optional-arglist)
		    ,(defmfun-body-only
		      (expand-defmfun-plain
		       nil
		       (append mandatory-arglist optional-arglist)
		       (second gsl-name)
		       (second c-arguments)
		       key-args))
		    ,(defmfun-body-only
		      (expand-defmfun-plain
		       nil
		       mandatory-arglist
		       (first gsl-name)
		       (first c-arguments)
		       key-args))))))))))

(defun defmfun-body-only (x)
  "Just get the body from the defmfun expansion."
  ;; no :documentation line!
  (nth 3 x))

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

(defun expand-defmfun-plain (name arglist gsl-name c-arguments key-args)
  (with-defmfun-key-args key-args
    (let* ((cargs (substitute '(mode sf-mode) :mode c-arguments))
	   (carg-symbs
	    (remove-if-not #'symbolp
			   (mapcar #'st-symbol (remove :mode c-arguments))))
	   (clargs (or arglist carg-symbs))
	   (arglist-symbs
	    (when arglist		; can be method, so get symbol
	      (mapcar (lambda (x) (if (listp x) (first x) x)) arglist)))
	   (cret-type (if (member c-return *special-c-return*)
			  :int
			  (if (listp c-return) (st-type c-return) c-return)))
	   (cret-name
	    (if (listp c-return) (st-symbol c-return) (make-symbol "CRETURN")))
	   (complex-args (complex-scalars clargs cargs))
	   (allocated		     ; Foreign objects to be allocated
	    (remove-if
	     (lambda (s)
	       (or (member s arglist-symbs) (member s global :key #'first)))
	     carg-symbs))
	   (allocated-decl
	    (append
	     (mapcar
	      (lambda (s) (find s cargs :key #'st-symbol))
	      allocated)))
	   (clret (or			; better as a symbol macro
		   (substitute cret-name :c-return return)
		   (mapcan #'cl-convert-form allocated-decl)
		   outputs
		   (unless (eq c-return :void)
		     (list cret-name))))
	   (clargs-types (cl-argument-types clargs cargs)))
      `(,defn
	,@(when (and name (not (defgeneric-method-p name)))
		(list name))
	,(let ((noaux
		(if (member :mode c-arguments)
		    `(,@clargs &optional (mode :double-prec))
		    `(,@clargs))))
	      (if global
		  (append noaux (cons '&aux global))
		  noaux))
	,(declaration-form clargs-types)
	,@(when documentation (list documentation))
	(,@(if (or allocated-decl complex-args)
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
			     (first (member (st-symbol arg) complex-args :key 'first))))
			(if cfind	; so substitute call to complex-to-gsl
			    `(,(third cfind)
			      (complex-to-gsl ,(first cfind) ,(second cfind)))
			    ;; otherwise use without conversion
			    (list (if (member (st-symbol arg) allocated)
				      :pointer
				      (st-type arg))
				  (st-symbol arg)))))
		    cargs)
		 ,cret-type)))
	   ,@(case c-return
		   (:void `((declare (ignore ,cret-name))))
		   (:error-code		; fill in arguments
		    `((check-gsl-status ,cret-name
		       ',(or (defgeneric-method-p name) name)))))
	   #-native
	   ,@(when outputs `(,(mapcar (lambda (x) `(setf (cl-invalid ,x) t))) outputs))
	   ,@(when (or null-pointer-info (eq c-return :pointer))
		   `((check-null-pointer ,cret-name
		      ,@(or null-pointer-info
			    '(:ENOMEM "No memory allocated")))))
	   ,@after
	   (values
	    ,@(defmfun-return
	       c-return cret-name clret allocated return return-supplied-p
	       enumeration outputs))))))))

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
		 (not allocated)
		 (not return-supplied-p))
	    (and (null return) return-supplied-p))
	 clret))))
