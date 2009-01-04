;; Use the foreign-friendly arrays package.
;; Liam Healy 2008-03-22 15:40:08EDT
;; Time-stamp: <2009-01-04 12:08:59EST foreign-friendly.lisp>
;; $Id$

;;; Foreign-friendly arrays (original implementation by Tamas Papp)
;;; http://www.princeton.edu/~tpapp/software.html
;;; The replacements here don't do coercions or checks
;;; on type as they are unnecessary, and handle complex types.

(in-package :gsl)

;;;;****************************************************************************
;;;; Make arrays for possible foreign use 
;;;;****************************************************************************

(defun make-ffa
    (element-type
     &key dimensions (initial-element nil initial-element-p)
     (initial-contents nil initial-contents-p)
     &allow-other-keys)
  "Make an array of one or two dimensions for possible use in foreign code.
   Syntax is similar to make-array, but note that element-type
   is mandatory and limited to certain types."
  (assert (member element-type *array-element-types* :test 'equal)
	  (element-type)
	  "Specified element-type must be one of *array-element-types*.")
  (let* ((initial-matrix (listp (first initial-contents)))
	 (complex-initial-real
	  (and initial-contents-p
	       (subtypep element-type 'complex)
	       (typep
		(if initial-matrix
		    (caar initial-contents)
		    (first initial-contents))
		'real))))
    (unless dimensions
      (if initial-contents-p
	  (setf dimensions
		(if initial-matrix
		    ;; matrix
		    (list (length initial-contents)
			  (ceiling (length (first initial-contents))
				   (if complex-initial-real 2 1)))
		    ;; vector
		    (ceiling (length initial-contents)
			     (if complex-initial-real 2 1))))
	  (error "dimensions must be specified if contents are not")))
    (when (and initial-matrix initial-contents-p)
      ;; flatten matrix spec
      (setf initial-contents (apply #'append initial-contents)))
    (when complex-initial-real
      (setf initial-contents (list-complex-from-real initial-contents)))
    (apply 'make-ffa-1d
	   dimensions element-type
	   (append
	    (when initial-contents-p
		     (list :initial-contents initial-contents))
	    (when initial-element-p
		     (list :initial-element initial-element))))))

(defun list-complex-from-real (list)
  "Collect a list of complex numbers from real components."
  (assert (evenp (length list)) (list) 
	  "Cannot collect reals into complex from an odd number of reals.")
  (loop for (re im) on list by #'cddr collect (complex re im)))

(defun make-ffa-1d (dimensions element-type &key
		 (initial-element 0 initial-element-p)
		 (initial-contents nil initial-contents-p))
  "Make an array that is either one-dimensional or displaced to a
   one-dimensional array.  Array is filled with initial-element or
   initial-contents, coerced to the given type."
  ;; element-type is a type spec in CL form
  (assert (or (atom dimensions) (and (listp dimensions) (car dimensions))))
  (let* ((dimensions (if (atom dimensions) (list dimensions) dimensions))
	 (length (reduce #'* dimensions))
	 (array (cond
		  ((and initial-element-p initial-contents-p)
		   (error "you can't supply both initial-element and ~
                           initial-contents"))
		  ;; initial element given
		  (initial-element-p
		   (make-array length :element-type element-type
			       :initial-element (coerce initial-element
							element-type)))
		  ;; contents given, copy or coerce
		  (initial-contents-p
		   (when (listp (first initial-contents))
		     ;; flatten list for matrices
		     (setf initial-contents (apply 'append initial-contents)))
		   (assert (= (length initial-contents) length))
		   (if (typep initial-contents (list 'vector element-type))
		       (copy-seq initial-contents)
		       (map (list 'vector element-type)
			    (lambda (x) (coerce x element-type)) initial-contents)))
		  ;; neither
		  (t (make-array length :element-type element-type)))))
    (if (cdr dimensions)
	(make-array dimensions :element-type element-type 
		    :displaced-to array)
	array)))

;;;;****************************************************************************
;;;; Protect native pointers (supercedes "pointer management" below for native)
;;;;****************************************************************************

;;; To be called by a defmfun expander
#+(and native sbcl)
(defun native-pointer-protect (array-symbols body)
  "Wrap the body with a form that obtains the native pointer
   and protects it during execution of the body."
  ;; http://www.sbcl.org/manual/Calling-Lisp-From-C.html
  `(sb-sys:with-pinned-objects
       ,(mapcar (lambda (s) `(original-array ,s)) array-symbols)
     ,body))

#+(and native sbcl)
(defun c-pointer (marray)
  "The pointer to the C array."
  (cffi:inc-pointer
   (sb-sys:vector-sap (original-array marray)) (offset marray)))

;;;;****************************************************************************
;;;; Pointer management
;;;;****************************************************************************

#+native
(defmacro with-pointer-to-array ((array pointer cffi-type length)
				 &body body)
  (assert (symbolp pointer))
  (once-only (array cffi-type)
    (with-unique-names (original-array index-offset)
      `(multiple-value-bind (,original-array ,index-offset)
	   (find-original-array ,array)
	 (pin-to-pointer (,original-array ,pointer ,cffi-type
					  ,length ,index-offset)
	   ,@body)))))

#-native
(defmacro with-pointer-to-array ((array pointer cffi-type length)
				 &body body)
  (assert (symbolp pointer))
  (once-only (array cffi-type)
    (with-unique-names (original-array index-offset)
      `(multiple-value-bind (,original-array ,index-offset)
	   (find-original-array ,array)
	 (cffi:with-foreign-object (,pointer ,cffi-type ,length)
	   ,@body)))))

#+(and native sbcl)
(defmacro pin-to-pointer ((array pointer cffi-type length index-offset)
			  &body body)
  (declare (ignorable length))
  "Use SBCL's sb-sys:with-pinned-objects and sb-sys:vector-sap for
mapping an array to a memory location.  NOTE: checking that cffi-type
matches the type of the array is the responsibility of the user of
this macro.  The size of the array is checked.  The array is required
to have rank one."
  (once-only (array)
    `(sb-sys:with-pinned-objects (,array)
       ;;(assert (<= (+ ,index-offset ,length) (length ,array)))
       (let ((,pointer
	      (cffi:inc-pointer
	       (sb-sys:vector-sap ,array)
	       (* ,index-offset (cffi:foreign-type-size ,cffi-type)))))
	 ,@body))))

(defun find-original-array (array)
  "Find the original parent of a displaced array, return this and the
sum of displaced index offsets."
  (let ((sum-of-offsets 0))
    (tagbody
     check-displacement
       (multiple-value-bind (displaced-to displaced-index-offset)
	   (array-displacement array)
	 (when displaced-to
	   (setf array displaced-to)
	   (incf sum-of-offsets displaced-index-offset)
	   (go check-displacement))))
    (values array sum-of-offsets)))
