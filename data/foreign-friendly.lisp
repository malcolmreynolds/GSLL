;; Use the foreign-friendly arrays package.
;; Liam Healy 2008-03-22 15:40:08EDT ffa.lisp
;; Time-stamp: <2008-09-14 16:16:50EDT foreign-friendly.lisp>
;; $Id$

;;; Use Papp's Foreign-friendly arrays
;;; http://www.princeton.edu/~tpapp/software.html

;;; Make a foreign-friendly array with #'make-array*,
;;; and/or use one in foreign code in a letm binding.


#|

There are five interrelated forms for these vectors:
  Native vector
  FFA objects made with with-ffa and used in with-pointer-to-array
  C array of doubles
  gsl-vector-c struct
  GSLL class vector-double-float
When to use what:
  GSL vectors gsl-vector-c used when GSL needs it.
  FFA used when on SBCL
  FFA used when C array needed and I don't want to make a
gsl-vector-c.  So maybe a good idea is to always have ffa available,
and only optionally make it part of GSL vectors.


Start implementing bare C array, which I don't actually use yet in
GSLL except in sort-vector-smallest-index, sort-vector-largest-index.
This can be written and tested without interfering with current code.

Add an object c-double-float (by analogy to vector-double-float) to letm processing. 
This will be set with standard aref; copy-array and the other things
defined in ffa will be usable.  

|#


(in-package :gsl)

;;;;****************************************************************************
;;;; A direct C array
;;;;****************************************************************************

(export '(make-array*))

(defun make-array*
    (dimensions element-type
     &key (initial-element nil initial-element-p) (initial-contents nil initial-contents-p))
  "Make an array for possible use in foreign code.
   Syntax is similar to make-array, but note that element-type
   is mandatory and limited to certain types."
  (assert (member element-type *array-element-types* :test 'equal)
	  (element-type) "Specified element-type must be one of *array-element-types*.")
  (if initial-contents-p
      (if (and (subtypep element-type 'complex)
	       (= (length initial-contents)
		  (* 2 (if (listp dimensions) (apply #'* dimensions) dimensions))))
	  (ffa:make-ffa dimensions element-type
			:initial-contents
			(loop for (re im) on initial-contents by #'cddr
			   collect (complex re im)))
	  (ffa:make-ffa dimensions element-type :initial-contents initial-contents))
      (if initial-element-p
	  (ffa:make-ffa dimensions element-type :initial-element initial-element)
	  (ffa:make-ffa dimensions element-type))))

(defparameter *foreign-array-directions*
  '((:in . :copy-in) (:out . :copy-out) (:in-out . :copy-in-out)))

(defmacro with-c-data
    ((pointer array cl-element-type &optional (direction :in-out))
     &body body)
  "Use an array made with make-array* in foreign code.  The pointer
   is either a symbol that will be bound to the foreign pointer, or
   a list of (c-pointer cl-object) so that the CL object created
   is also accessible."
  (let ((cl-name (if (listp pointer) (second pointer) (gensym "ARRAY"))))
    `(let ((,cl-name ,array))
      (ffa:with-pointer-to-array
	  (,cl-name
	   ,(if (listp pointer) (first pointer) pointer)
	   (cl-ffa ',cl-element-type)
	   (array-total-size ,cl-name)
	   ,(rest (assoc direction *foreign-array-directions*)))
	,@body))))

;;;;****************************************************************************
;;;; Expand letm bindings
;;;;****************************************************************************

;;; Modify ffa's array-copy to take type argument
(defun array-copy (array &optional type)
  "Copy the elements of array.  Does not copy the elements itself
  recursively, if you need that, use array-map."
  (ffa:make-ffa (array-dimensions array)
		(or type (array-element-type array))
		:initial-contents (ffa:find-or-displace-to-flat-array array)))

(defun expand-direct-c (symbol cl-element-type init-or-spec body)
  "Expand the form for a direct foreign array.
   Argument 'symbol can be either a symbol or a list of
   two symbols; if the former, it will be bound to the foreign pointer; if
   the latter, the first symbol will be bound to the foreign pointer and
   the second to the corresponding CL array.  The cl-element-type
   is a CL element type, init-or-spec is either the
   array dimensions or something made by make-array*."
  (typecase init-or-spec
    (array
     `(with-c-data
       (,symbol (array-copy ,init-or-spec ',cl-element-type) ,cl-element-type)
       ,@body))
    ((or list number)
     `(with-c-data
       (,symbol
	(make-array* ,init-or-spec ',cl-element-type)
	,cl-element-type)
       ,@body))
    (t				     ; unknown at macro expansion time
     (cl-utilities:once-only (init-or-spec)
       `(with-c-data
	 (,symbol
	  (if (typep ,init-or-spec 'array)
	      ,init-or-spec
	      (make-array* ,init-or-spec ',cl-element-type))
	  ,cl-element-type)
	 ,@body)))))

(defmethod letm-expansion (symbol (type (eql 'c-double-float)) args body)
  (expand-direct-c symbol 'double-float (first args) body))

(arglist-only c-double-float 
	      "A direct C vector or matrix of type double."
	      dimensions-or-init)

#|
;; Example
(letm ((a (c-double-float 3)) ((b cl-b) (c-double-float #(3 4 5 1 2))))
  (format t "b: foreign pointer=~a~%" b)
  (format t "a: foreign pointer=~a~%" a)
  (values (aref cl-b 3) (dcref b 3)))

(LET ((#:ARRAY3416 (MAKE-ARRAY* 3 'DOUBLE-FLOAT)))
  (FFA:WITH-POINTER-TO-ARRAY
      (#:ARRAY3416 A (CL-FFA 'DOUBLE-FLOAT)
		   (ARRAY-TOTAL-SIZE #:ARRAY3416) :COPY-IN-OUT)
    (LET ((CL-B (ARRAY-COPY #(3 4 5 1 2) 'DOUBLE-FLOAT)))
      (FFA:WITH-POINTER-TO-ARRAY
	  (CL-B B (CL-FFA 'DOUBLE-FLOAT) (ARRAY-TOTAL-SIZE CL-B)
		:COPY-IN-OUT)
	(FORMAT T "b: foreign pointer=~a~%" B)
	(FORMAT T "a: foreign pointer=~a~%" A) (VALUES (AREF CL-B 3) (DCREF B 3))))))
|#
