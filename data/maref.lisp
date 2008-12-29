;; Get/set array or elements: cl-array, maref
;; Liam Healy 2008-08-27 22:43:10EDT maref.lisp
;; Time-stamp: <2008-12-28 18:09:19EST maref.lisp>
;; $Id: $

(in-package :gsl)

(export '(cl-array maref))

;;; These functions handle the details of converting between the GSL
;;; representation of arrays and Common Lisp arrays.  The function
;;; #'maref can be treated like #'aref for reading and setting
;;; elements, except that it is limited to two indices (vectors or
;;; matrices only) and requires a final argument with the element
;;; type when taking a GSL pointer as the first argument (only needed
;;; in the case of solve-minimize-fit for callbacks and some return
;;; values).

;;; Both these functions take one of the following class arguments for
;;; the array:
;;; - a foreign-array
;;; - a Common Lisp array
;;; - a pointer to a GSL vector or matrix structure

;;;;****************************************************************************
;;;; Get the entire array:  cl-array
;;;;****************************************************************************

(defgeneric cl-array (object &optional array-rank element-type)
  (:documentation
   "The array as a CL native array.  The object may be a foreign-array object,
    a pointer to a GSL vector or matrix, or an ordinary CL array of one
    or two dimensions.  Optional arguments array-rank and element-type
    are used only for pointers.")
  (:method ((object foreign-array) &optional array-rank element-type)
    (declare (ignore array-rank element-type))
    #-native (copy-c-to-cl object)
    (slot-value object 'cl-array))
  (:method ((object array) &optional array-rank element-type)
    ;; For compatibility, work on CL arrays as well.
    (declare (ignore array-rank element-type))
    object))

;;;;****************************************************************************
;;;; Get or set elements of the array:  maref, (setf maref)
;;;;****************************************************************************

(defgeneric maref (object index &optional index2 type)
  (:documentation
   "An element of the data.  The object may be a foreign-array object, a pointer to
    a GSL vector or matrix, or an ordinary CL array of one or two dimensions.")
  (:method ((object foreign-array) index &optional index2 type)
    (declare (ignore type))
    #-native (copy-c-to-cl object)
    (if index2
	(aref (cl-array object) index index2)
	(aref (cl-array object) index)))
  (:method ((object array) index &optional index2 type)
    ;; For compatibility, work on CL arrays as well.
    (declare (ignore type))
    (if index2
	(aref object index index2)
	(aref object index))))

;;; Alternative to complete copy is to mark which elements have
;;; changed and just copy them.  Is it worth it?

(defgeneric (setf maref) (value object index &optional index2 type)
  (:documentation
   "Set an element of the data.  The object may be a foreign-array object,
    a pointer to a GSL vector or matrix, or an ordinary CL array
    of one or two dimensions.")
  (:method (value (object foreign-array) index &optional index2 type)
    (declare (ignore type))
    (if index2
	(setf (aref (slot-value object 'cl-array) index index2) value)
	(setf (aref (slot-value object 'cl-array) index) value))
    #-native (setf (c-invalid object) t))
  (:method (value (object array) index &optional index2 type)
    ;; For compatibility, work on CL arrays as well.
    (declare (ignore type))
    (if index2
	(setf (aref object index index2) value)
	(setf (aref object index) value))))

;;; Normally we won't need to set or get directly from the GSL
;;; vector/matrix pointer.  However, it is necessary to have access to
;;; elements from the GSL vector/matrix pointers for things like
;;; callback functions in solve-minimize-fit and in #'cl-array method
;;; for pointers.

(eval-when (:compile-toplevel :load-toplevel)
(defun maref-function-picker (type-symbol category ffrestargs &optional value-symbol)
  "Generate sexp to select on the various gsl_{vector,matrix}*_{get,set} functions."
  (cons 'cond
	(mapcar (lambda (tp)
		  `((equal ,type-symbol ',tp)
		    (cffi:foreign-funcall
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,(if value-symbol "_set" "_get"))
		       category tp)
		     ,@ffrestargs
		     ,@(if value-symbol
			   (list (cl-cffi tp) value-symbol)
			   (list (cl-cffi tp))))))
		*array-element-types*))))

(defmethod maref
    ((pointer #.+foreign-pointer-class+) index &optional index2
     (type 'double-float))
  (if index2
      #.(maref-function-picker
	 'type 'matrix
	 '(:pointer pointer sizet index sizet index2))
      #.(maref-function-picker
	 'type 'vector
	 '(:pointer pointer sizet index))))

;;; Index the GSL function names to maref
#.(cons 'progn
	(append
	 (mapcar (lambda (tp)
		   `(map-name
		     'maref
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_get")
		       'vector tp)))
		 *array-element-types*)
	 (mapcar (lambda (tp)
		   `(map-name
		     'maref
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_get")
		       'matrix tp)))
		 *array-element-types*)))

(defmethod (setf maref)
    (value (pointer #.+foreign-pointer-class+) index &optional index2
     (type 'double-float))
  (if index2
      #.(maref-function-picker
	 'type 'matrix
	 '(:pointer pointer sizet index sizet index2) 'value)
      #.(maref-function-picker
	 'type 'vector
	 '(:pointer pointer sizet index) 'value)))

;;; Index the GSL function names to (setf maref)
#.(cons 'progn
	(append
	 (mapcar (lambda (tp)
		   `(map-name
		     '(setf maref)
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_set")
		       'vector tp)))
		 *array-element-types*)
	 (mapcar (lambda (tp)
		   `(map-name
		     '(setf maref)
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_set")
		       'matrix tp)))
		 *array-element-types*)))




