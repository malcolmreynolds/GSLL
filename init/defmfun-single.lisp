;; Helpers that define a single GSL function interface
;; Liam Healy 2009-01-07 22:02:20EST defmfun-single.lisp
;; Time-stamp: <2009-04-11 21:24:36EDT defmfun-single.lisp>
;; $Id: $

(in-package :gsl)

(defun defgeneric-method-p (name)
  "When defining :method in a defgeneric, (nil foo) is used for
   the name, and foo will be returned from this function."
  (if (and (listp name) (null (first name)))
      (second name)))

(defun complex-scalars (cl-arguments c-arguments-types)
  "Create a list of (symbol temp-variable c-struct) for each complex
  variable in the argument."
  (loop for sd in c-arguments-types
     for cl-type = (cffi-cl (st-type sd))
     append
     (when (and cl-type (subtypep cl-type 'complex)
		(member (st-symbol sd)
			(arglist-plain-and-categories cl-arguments)))
       (list (list (st-symbol sd)
		   (make-symbol (string (st-symbol sd)))
		   (st-type sd))))))

(defun substitute-symbols (substlist list)
  "Substitute in list the second element of each list of substlist for
   the first element."
  (loop for pair in substlist
     with answer = list
     do
     (setf answer
	   (subst (second pair) (first pair) answer))
     finally (return answer)))

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
		   (mappend #'scwfv (rest sexp))))))
    (remove nil (remove-duplicates (scwfv sexp)))))

(defun variables-used-in-c-arguments (c-arguments)
  "Find the arguments passed to the C function.  This is a poor
  quality code walker, but is sufficient for actual usage of defmfun."
  (remove-duplicates
   (mappend (lambda (carg)
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

(define-condition obsolete-gsl-version (error)
  ((name :initarg :name :reader name)
   (gsl-name :initarg :gsl-name :reader gsl-name)
   (gsl-version :initarg :gsl-version :reader gsl-version))
  (:report
   (lambda (condition stream)
     (apply 'format stream
	    "Function ~a (~a) is not available in the ~
               ~%currently loaded release ~a of GSL; it was introduced in release ~d.~d."
	    (name condition)
	    (gsl-name condition)
	    *gsl-version*
	    (gsl-version condition))))
  (:documentation
   "An error indicating that the currently loaded version of the GSL libary
    does not have the function defined."))

(defun complete-definition
    (defn name arglist gsl-name c-arguments key-args
     &optional
     (body-maker 'body-no-optional-arg)
     (mapdown (eq body-maker 'body-optional-arg)))
  "A complete definition form, starting with defun, :method, or defmethod."
  (with-defmfun-key-args key-args
    (if (have-at-least-gsl-version gsl-version)
	`(,defn
	     ,@(when (and name (not (defgeneric-method-p name)))
		     (list name))
	     ,@(when qualifier (list qualifier))
	   ,arglist
	   ,(declaration-form
	     (cl-argument-types arglist c-arguments)
	     (set-difference	       ; find all the unused variables
	      (arglist-plain-and-categories arglist nil)
	      (remove-duplicates (union
	       (if mapdown
		   (apply 'union
			  (mapcar 'variables-used-in-c-arguments c-arguments))
		   (variables-used-in-c-arguments c-arguments))
	       ;; Forms in :before, :after are checked for used variables
	       (stupid-code-walk-find-variables
		(cons
		 'values
		 (append before after
			 (callback-symbol-set
			  callback-dynamic cbinfo (first callback-dynamic-variables))
			   ;; &optional/&key/&aux defaults are checked
			 (let ((auxstart (after-llk arglist)))
			   (when auxstart
			     (apply
			      'append
			      (mapcar 'rest (remove-if 'atom auxstart)))))))))))
	     (first callback-dynamic-variables))
	   ,@(when documentation (list documentation))
	   ,(funcall body-maker name arglist gsl-name c-arguments key-args))
	`(,defn
	     ,@(when (and name (not (defgeneric-method-p name)))
		     (list name))
	     ,@(when qualifier (list qualifier))
	   ,arglist
	   (declare (ignorable ,@(arglist-plain-and-categories arglist nil)))
	   (error 'obsolete-gsl-version
		  :name ',name :gsl-name ',gsl-name :gsl-version ',gsl-version)))))

(defun wrap-letlike (when binding wrapping body)
  (if when
      `(,wrapping ,binding ,@body)
      (if (eql (length body) 1)
	  (first body)
	  `(progn ,@body))))

(defun body-no-optional-arg (name arglist gsl-name c-arguments key-args)
  "Wrap necessary array-handling forms around the expanded unswitched
  body form."
  #-native
  (destructuring-bind (&key inputs &allow-other-keys) key-args
    `(progn
       ,@(mapcar (lambda (v) `(copy-cl-to-c ,v))
		 (intersection
		  inputs (arglist-plain-and-categories arglist nil)))
       ,(body-expand name arglist gsl-name c-arguments key-args)))
  #+native
  (destructuring-bind (&key inputs outputs &allow-other-keys) key-args
    (native-pointer
     (intersection
      (union inputs outputs) (arglist-plain-and-categories arglist nil))
     (body-expand name arglist gsl-name c-arguments key-args))))

(defun body-expand (name arglist gsl-name c-arguments key-args)
  "Expand the body (computational part) of the defmfun."
  (with-defmfun-key-args key-args
    (let* ((cret-type (if (member c-return *special-c-return*)
			  :int
			  (if (listp c-return) (st-type c-return) c-return)))
	   (cret-name
	    (if (listp c-return) (st-symbol c-return) (make-symbol "CRETURN")))
	   (complex-args (complex-scalars arglist c-arguments))
	   (cargs (substitute-symbols complex-args c-arguments)) ; with complex
	   (allocated		     ; Foreign objects to be allocated
	    (remove-if
	     (lambda (s)
	       (member s (arglist-plain-and-categories arglist nil)))
	     (variables-used-in-c-arguments cargs)))
	   (allocated-decl
	    (mapcar
	     (lambda (s)
	       (or (find s cargs :key #'st-symbol)
		   ;; Catch programming errors, usually typos
		   (error "Could not find ~a among the arguments" s)))
	     allocated))
	   ;; same as allocated-decl, but no complex struct variables
	   (allocated-decl-ret
	    (mapcar
	     (lambda (s)
	       (or (find s (set-difference c-arguments (mapcar 'first complex-args))
			 :key #'st-symbol)
		   ;; Catch programming errors, usually typos
		   (error "Could not find ~a among the arguments" s)))
	     (set-difference allocated  (mapcar 'second complex-args))))	   
	   (clret (or			; better as a symbol macro
		   (substitute
		    cret-name :c-return
		    (mapcar (lambda (sym)
			      (let ((it (find sym allocated-decl-ret :key 'st-symbol)))
				(if it
				    (first (cl-convert-form it))
				    sym)))
			    return))
		   (mappend
		    #'cl-convert-form
		    (callback-remove-arg allocated-decl-ret cbinfo 'st-symbol))
		   outputs
		   (unless (eq c-return :void)
		     (list cret-name)))))
      (wrap-letlike
       allocated-decl
       (mapcar (lambda (d) (wfo-declare d cbinfo))
	       allocated-decl)
       'cffi:with-foreign-objects
       `(,@(append
	    (callback-symbol-set
	     callback-dynamic cbinfo (first callback-dynamic-variables))
	    before
	    (when complex-args
	      (loop for arg in complex-args
		 collect `(cl-to-complex ,(first arg) ,(second arg))))
	    (when callback-object (callback-set-dynamic callback-object arglist)))
	   ,@(callback-set-slots
	      cbinfo callback-dynamic-variables callback-dynamic)
	   (let ((,cret-name
		  (,(if complex-args
			'fsbv:foreign-funcall
			'cffi:foreign-funcall)
		    ,gsl-name
		    ,@(mappend
		       (lambda (arg)
			 (list (cond
				 ((third (find (st-symbol arg) complex-args :key 'second)))
				 ((member (st-symbol arg) allocated) :pointer)
				 (t (st-type arg)))
			       (st-symbol arg)))
		       cargs)
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
		 c-return cret-name clret
		 (set-difference allocated (mapcar 'second complex-args))
		 return return-supplied-p
		 enumeration outputs))))))))

(defun defmfun-return
    (c-return cret-name clret allocated return return-supplied-p enumeration outputs)
  "Generate the return computation expression for defmfun."
  (case c-return
    (:number-of-answers
     (mappend
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
