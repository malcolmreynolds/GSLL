;; Generate a wrapper to be called by callback, that calls user function.
;; Liam Healy 
;; Time-stamp: <2009-03-25 21:11:29EDT funcallable.lisp>
;; $Id$

(in-package :gsl)

;; To do: change the specification scheme for each argument to 
;; (direction type array-type &rest dimensions)

(defun callback-args (types)
  "The arguments passed by GSL to the callback function."
  (mapcar (lambda (type)
	    (let ((symbol (gensym "ARG")))
	      (list symbol
		    (if (listp type)	; like (:double 3)
			:pointer	; C array
			type))))
	  (if (listp types) types (list types))))

;;; (embedded-clfunc-args '(:double (:double 2) (:set :double 2)) (callback-args '(:double (:double 2) (:set :double 2))))
;;; (#:ARG1244 (MEM-AREF #:ARG1245 ':DOUBLE 0) (MEM-AREF #:ARG1245 ':DOUBLE 1))

(defvar *setting-spec* '(:set))
(defun embedded-clfunc-args (types callback-args &optional marray)
  "The arguments passed to the CL function call embedded in the callback.
   If 'marray is T, then reference GSL arrays; otherwise reference raw
   C vectors.  A specification (:set ...) means that the CL function
   will define the array as multiple values; if the size is negative,
   then the opposite value will be used for marray."
  (loop for spec in types
     for (symbol nil) in callback-args
     append
     (unless (and (listp spec) (member (first spec) *setting-spec*))
       (if (listp spec)
	   (if (third spec)
	       ;; matrix, marrays only
	       (loop for i from 0 below (second spec)
		  append
		  (loop for j from 0 below (third spec)
		     collect
		     `(maref ,symbol ,i ,j ',(cffi-cl (first spec)))))
	       ;; vector, marray or C array
	       (loop for ind from 0 below (abs (second spec))
		  collect (if (if (minusp (second spec)) (not marray) marray)
			      `(maref ,symbol ,ind nil ',(cffi-cl (first spec)))
			      `(cffi:mem-aref ,symbol ',(first spec) ,ind))))
	   (list symbol)))))

(defun callback-set-mvb (form types callback-args &optional marray)
  "Create the multiple-value-bind form in the callback to set the return C arrays."
  (multiple-value-bind (settype setcba)
      (loop for cba in callback-args
	 for type in types
	 for setting = (and (listp type) (member (first type) *setting-spec*))
	 when setting
	 collect cba into setcba
	 when setting
	 collect type into settype
	 finally (return (values (mapcar 'rest settype) setcba)))
    (let* ((setvbls (embedded-clfunc-args settype setcba marray))
	   (count
	    (apply
	     '+
	     (mapcar (lambda (inds) (abs (apply '* (rest inds)))) settype)))
	   (mvbvbls (loop repeat count collect (gensym "SETCB"))))
      (if (zerop count)
	  form
	  `(multiple-value-bind ,mvbvbls
	       ,form
	     (setf ,@(loop for mvbvbl in mvbvbls
			for setvbl in setvbls
			append (list setvbl mvbvbl))))))))

(defun make-funcallable
    (dynfn-variable &optional (return-type :double) (argument-types :double)
     additional-argument-types marray)
  "Define a wrapper function to interface GSL with the user's function"
  (let* ((atl (if (listp argument-types) argument-types (list argument-types)))
	 (aatl (if (listp additional-argument-types) additional-argument-types
		   (list additional-argument-types)))
	 (cbargs (callback-args atl))
	 (cbaddl (callback-args aatl)))
    `(lambda
	 (,@(mapcar 'st-symbol cbargs) params ,@(mapcar 'st-symbol cbaddl))
       ;; Parameters as C argument are always ignored, because we have
       ;; CL specials to do the same job.
       (declare (ignore params) (special ,dynfn-variable))
       ,(callback-set-mvb
	 `(funcall
	   (first ,dynfn-variable)
	   ,@(append
	      (embedded-clfunc-args atl cbargs marray)
	      (embedded-clfunc-args aatl cbaddl marray)))
	 (append atl aatl)
	 (append cbargs cbaddl)
	 marray)
       ,@(case
	  return-type
	  (:success-failure
	   ;; We always return success, because if there was a
	   ;; problem, a CL error would be signalled.
	   '(+success+))
	  (:pointer
	   ;; For unclear reasons, some GSL functions want callbacks
	   ;; to return a void pointer which is apparently meaningless.
	   '((cffi:null-pointer)))))))
