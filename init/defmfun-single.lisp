;; Helpers that define a single GSL function interface
;; Liam Healy 2009-01-07 22:02:20EST defmfun-single.lisp
;; Time-stamp: <2009-04-13 22:09:35EDT defmfun-single.lisp>
;; $Id: $

(in-package :gsl)

(defun defgeneric-method-p (name)
  "When defining :method in a defgeneric, (nil foo) is used for
   the name, and foo will be returned from this function."
  (if (and (listp name) (null (first name)))
      (second name)))

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
