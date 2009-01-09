;; Helpers for defining GSL functions on arrays
;; Liam Healy 2009-01-07 22:01:16EST defmfun-array.lisp
;; Time-stamp: <2009-01-08 22:02:50EST defmfun-array.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; A defgeneric with methods, or the methods alone, for vectors or matrices
;;;;****************************************************************************

(defun expand-defmfun-arrays
    (defn name arglist gsl-name c-arguments categories key-args)
  (setf categories
	(remove-if-not
	 (lambda (c) (member c '(both matrix vector)))
	 categories))
  (if (member 'both categories)
      (progn 
	(when (> (length categories) 1)
	  ;; Specify 'both alone
	  (error "Internal: mixed both and actual category."))
	;; Generate forms for matrix and vector
	(append
	 (generate-methods
	  defn 'vector
	  name arglist gsl-name
	  (actual-array-c-type
	   'vector c-arguments
	   (optional-args-to-switch-gsl-functions arglist gsl-name))
	  (copy-list key-args) 'vector)
	 (generate-methods
	  defn 'matrix
	  name arglist gsl-name
	  (actual-array-c-type
	   'matrix c-arguments
	   (optional-args-to-switch-gsl-functions arglist gsl-name))
	  (copy-list key-args) 'matrix)))
      ;; Generate forms for one category
      (generate-methods
       defn (first categories)
       name arglist gsl-name c-arguments key-args)))

(defun expand-defmfun-generic (name arglist gsl-name c-arguments key-args)
  "Define a generic function and methods for all kinds of arrays."
  ;; Need to scan the arglist for categories.
  ;; Can be mixed, unless it says 'both, in which case it can only be both.
  (with-defmfun-key-args key-args
    (multiple-value-bind (noclass-arglist categories)
	(arglist-plain-and-categories arglist)
      `(defgeneric ,name ,noclass-arglist
	 (:documentation ,documentation)
	 ,@(expand-defmfun-arrays
	    :method name arglist gsl-name c-arguments categories key-args)))))

(defun expand-defmfun-defmethods (name arglist gsl-name c-arguments key-args)
  "Define methods for all kinds of arrays."
  (let ((categories (nth-value 1 (arglist-plain-and-categories arglist))))
    (expand-defmfun-arrays
     'cl:defmethod name arglist gsl-name c-arguments categories key-args)))

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
		(if (optional-args-to-switch-gsl-functions arglist gsl-name)
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
	 ;; Default values for optional/key arguments are treated
	 ;; specially for array methods.
	 (if (and (listp arg) (numberp (second arg)))
	     (list (first arg)	; Optional arg default numerical value
		   ;; coerce to the right type
		   (coerce (second arg) element-type))
	     (if (listp arg)
		 (if (and (listp (second arg))
			  (eq (first (second arg)) 'eltcase))
		     ;; "eltcase" switch to the matching form
		     `(,(first arg)
			,(element-type-select (rest (second arg)) element-type))
		     ;; 'element-type is bound to the element-type.
		     (list
		      (first arg)
		      (subst `',element-type 'element-type
			     (second arg))))
		 arg)))))

(defun element-type-select (form element-type)
  "Find the actual form to use as the default based on the list in form."
  (loop for (type result) on form by #'cddr
     thereis (when (subtypep element-type type) result)))
     
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
      ;; argument, then map this function onto each.
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
