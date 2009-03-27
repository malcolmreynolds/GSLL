;; Generate a wrapper to be called by callback, that calls user function.
;; Liam Healy 
;; Time-stamp: <2009-03-26 23:19:47EDT funcallable.lisp>
;; $Id$

(in-package :gsl)

(defun make-symbol-cardinal (name i)
  (make-symbol (format nil "~:@(~a~)~d" name i)))

(defun reference-foreign-element (foreign-variable-name index argspec)
  "Form to reference, for getting or setting, the element of a foreign
   array, or a scalar."
  (if (parse-callback-argspec argspec 'dimensions)
      (if (eql (parse-callback-argspec argspec 'dimensions) :marray)
	  `(maref
	    ,foreign-variable-name
	    ;; need to pad NIL for vectors
	    ,@(parse-callback-argspec argspec 'dimensions)
	    ',(cffi-cl (parse-callback-argspec argspec 'element-type)))
	  `(cffi:mem-aref
	    ,foreign-variable-name
	    ',(parse-callback-argspec argspec 'element-type)
	    ,index))
      ;; not setfable if it's a scalar
      foreign-variable-name))

(defun callback-set-mvb (argument-names form fnspec)
  "Create the multiple-value-bind form in the callback to set the return C arrays."
  (let* ((setargs		 ; arguments that are arrays being set
	  (remove-if-not
	   (lambda (arg)
	     (and (eql (parse-callback-argspec arg 'io) :output)
		  (parse-callback-argspec arg 'dimensions)))
	   (parse-callback-fnspec fnspec 'arguments-spec)))
	 (counts		  ; number of scalars for each set arg
	  (mapcar
	   (lambda (arg)
	     (abs (apply '* (parse-callback-argspec arg 'dimensions))))
	   setargs))
	 (count				; total number of scalars set
	  (apply '+ counts))
	 (mvbvbls	      ; the symbols to be multiple-value-bound
	  (loop for i from 0 below count
	     collect (make-symbol-cardinal 'setscalar i)))
	 (setvbls
	  (loop for arg in setargs
	     for count in counts
	     append
	     (loop for i from 0 below count
		collect (reference-foreign-element argument-names i arg)))))
    (if (zerop count)
	form
	`(multiple-value-bind ,mvbvbls
	     ,form
	   (setf ,@(loop for mvbvbl in mvbvbls
		      for setvbl in setvbls
		      append (list setvbl mvbvbl)))))))

(defun make-funcallable (dynfn-variable fnspec)
  "Define a wrapper function to interface GSL with the user's function"
  (let* ((argspecs (parse-callback-fnspec fnspec 'arguments-spec))
	 (noslug (remove :slug argspecs))
	 (paramsarg (make-symbol "PARAMS"))
	 (lambda-args
	  (loop for arg in argspecs
	     with count = 0
	     collect
	     (if (eql arg :slug)
		 paramsarg
		 (progn
		   (if (eql (parse-callback-argspec arg 'io) :output)
		       (make-symbol-cardinal 'output count)
		       (make-symbol-cardinal 'input count))
		   (incf count)))))
	 (call-form
	  `(funcall
	    (first ,dynfn-variable)
	    ,@(loop ;; over all elements of input variables
		 (reference-foreign-element ???????)))))
    `(lambda
	 (,lambda-args)
       ;; Parameters as C argument are always ignored, because we have
       ;; CL specials to do the same job.
       (declare (ignore ,paramsarg) (special ,dynfn-variable))
       ,(if (find :output argspecs
		  :key (lambda (arg) (parse-callback-argspec arg 'io)))
	    ,(callback-set-mvb
	      argument-names
	      call-form
	      fnspec)
	    ;; no specified output, return what the function returns
	    call-form)
       ,@(case
	  (parse-callback-fnspec fnspec 'return-spec)
	  (:success-failure
	   ;; We always return success, because if there is a
	   ;; problem, a CL error should be signalled.
	   '(+success+))
	  (:pointer
	   ;; For unclear reasons, some GSL functions want callbacks
	   ;; to return a void pointer which is apparently meaningless.
	   '((cffi:null-pointer)))
	  ;; If it isn't either of these things, return what the
	  ;; function returned.
	  (otherwise nil)))))
