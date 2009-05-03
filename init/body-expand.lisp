;; Expand the body of a defmfun
;; Liam Healy 2009-04-13 22:07:13EDT body-expand.lisp
;; Time-stamp: <2009-05-03 10:03:03EDT body-expand.lisp>
;; $Id: $

(in-package :gsl)

(defun creturn-st (c-return)
  "The symbol-type of the return from the C function."
  (make-st
   (if (listp c-return)
       (st-symbol c-return)
       (make-symbol "CRETURN"))
   (if (member c-return *special-c-return*)
       :int
       (if (listp c-return) (st-type c-return) c-return))))

(defparameter *c-types-passed-by-value*
  '(complex-float-c complex-double-c))

(defun variables-passed-by-value (c-arguments-st)
  "Create a list of (symbol newsymbol-st) for each structure
  variable in the C argument list."
  (loop for sd in c-arguments-st
     collect
     (when (member (st-type sd) *c-types-passed-by-value*)
       (list (st-symbol sd)
	     (make-st
	      (make-symbol (string (st-symbol sd)))
	      (st-type sd))))))

(defun substitute-symbols (substlist st-list)
  "Substitute in the new symbols for the old in the st list."
  (loop for pair in substlist
     with answer = st-list
     do
     (setf answer
	   (subst
	    (st-symbol (second pair))
	    (first pair)
	    answer
	    :test 'st-symbol))
     finally (return answer)))

;;; This function should never be called even when FSBV is absent,
;;; because the potential callers should all have #-fsbv
;;; conditionalization.  It is here just so that body-expand can
;;; compile when FSBV is absent.
#-fsbv
(defmacro no-fsbv-error (function-name &rest args)
  (declare (ignore args))
  `(error
    "System FSBV is not present, so function ~a cannot be used."
    ,function-name))

(defun body-expand (name arglist gsl-name c-arguments key-args)
  "Expand the body (computational part) of the defmfun."
  (with-defmfun-key-args key-args
    (let* ((creturn-st (creturn-st c-return))
	   (allocated-return ; Allocated and then returned from CL function
	    (mapcar
	     (lambda (s)
	       (or (find s c-arguments :key #'st-symbol)
		   ;; Catch programming errors, usually typos
		   (error "Could not find ~a among the arguments" s)))
	     (remove-if
	      (lambda (s)
		(member s (arglist-plain-and-categories arglist nil)))
	      (variables-used-in-c-arguments c-arguments))))
	   (pbv (variables-passed-by-value c-arguments)) ; passed by value
	   (clret (or			; better as a symbol macro
		   (substitute
		    (st-symbol creturn-st) :c-return
		    (mapcar (lambda (sym)
			      (let ((it (find sym allocated-return :key 'st-symbol)))
				(if it
				    (first (cl-convert-form it))
				    sym)))
			    return))
		   (mappend
		    #'cl-convert-form
		    (callback-remove-arg allocated-return cbinfo 'st-symbol))
		   outputs
		   (unless (eq c-return :void)
		     (list (st-symbol creturn-st))))))
      (wrap-letlike
       allocated-return
       (mapcar (lambda (d) (wfo-declare d cbinfo))
	       allocated-return)
       'cffi:with-foreign-objects
       `(,@(append
	    (callback-symbol-set
	     callback-dynamic cbinfo (first callback-dynamic-variables))
	    before
	    (when callback-object (callback-set-dynamic callback-object arglist)))
	   ,@(callback-set-slots
	      cbinfo callback-dynamic-variables callback-dynamic)
	   (let ((,(st-symbol creturn-st)
		  (,(if (some 'identity pbv)
			#+fsbv 'fsbv:foreign-funcall
			#-fsbv 'no-fsbv-error
			'cffi:foreign-funcall)
		    ,gsl-name
		    ,@(mappend
		       (lambda (arg)
			 (list (cond
				 ((member (st-symbol arg) allocated-return) :pointer)
				 (t (st-type arg)))
			       (st-symbol arg)))
		       (mapcar 'st-pointer-generic-pointer c-arguments))
		    ,(st-type creturn-st))))
	     ,@(case c-return
		     (:void `((declare (ignore ,(st-symbol creturn-st)))))
		     (:error-code	; fill in arguments
		      `((check-gsl-status ,(st-symbol creturn-st)
					  ',(or (defgeneric-method-p name) name)))))
	     #-native
	     ,@(when outputs
		     (mapcar
		      (lambda (x) `(setf (cl-invalid ,x) t (c-invalid ,x) nil))
		      outputs))
	     ,@(when (eq (st-type creturn-st) :pointer)
		     `((check-null-pointer
			,(st-symbol creturn-st)
			,@'('memory-allocation-failure "No memory allocated."))))
	     ,@after
	     (values
	      ,@(defmfun-return
		 c-return (st-symbol creturn-st) clret
		 allocated-return
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

