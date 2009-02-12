;; Copy marrays to/from CL arrays
;; Liam Healy 2009-02-11 19:28:44EST copy-cl.lisp
;; Time-stamp: <2009-02-11 23:00:07EST copy-cl.lisp>
;; $Id: $

;;; The function #'copy can be used to copy the contents to or from a
;;; CL array.  If the destination isn't literally supplied, it can be
;;; a symbol 'array indicating that the marray is to be copied to a CL
;;; array, or an element type like 'double-float, indicating the
;;; contents of the CL array is to be copied to a new marray with the
;;; given element type.

;;; Contrast this with the function #'cl-array or the initarg
;;; :cl-array to #'make-marray.  The function returns the cl-array
;;; without copying, and so might be faster but also makes the
;;; original marray object vulnerable if an element is changed, which
;;; officially has unpredictable results.  Likewise, the initarg will
;;; use the given CL array directly in the created marray without
;;; copying.

(in-package :gsl)

(define-condition array-mismatch (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Arrays must be of equal size for copying.")))
  (:documentation
   "An error indicating that the two arrays do not have 
   the same dimensions."))

(defmethod copy-to-destination ((source mvector) (destination array))
  (unless (equal (dimensions source) (array-dimensions destination))
    (error 'array-mismatch))
  (loop for i below (dim0 source)
       do (setf (aref destination i) (maref source i)))
  destination)

(defmethod copy-to-destination ((source matrix) (destination array))
  (unless (equal (dimensions source) (array-dimensions destination))
    (error 'array-mismatch))
  (loop for i below (dim0 source) do
       (loop for j below (dim1 source) do
	    (setf (aref destination i j) (maref source i j))))
  destination)

(defmethod copy-to-destination ((source array) (destination mvector))
  (unless (equal (array-dimensions source) (dimensions destination))
    (error 'array-mismatch))
  (loop for i below (length source)
       do (setf (maref destination i) (aref source i)))
  destination)

(defmethod copy-to-destination ((source array) (destination matrix))
  (unless (equal (array-dimensions source) (dimensions destination))
    (error 'array-mismatch))
  (loop for i below (array-dimension source 0) do
       (loop for j below (array-dimension source 1) do
	    (setf (maref destination i j) (aref source i j))))
  destination)

;;; These make destination but are methods of copy-to-destination so
;;; that the class argument may be supplied.

(defmethod copy-to-destination ((source marray) (destclass (eql 'array)))
  (let ((destination
	 (make-ffa (element-type source) :dimensions (dimensions source))))
    (copy-to-destination source destination)))

;;; Copy to a named marray element-type, where the type is a symbol
(defmethod copy-to-destination ((source array) (destclass symbol))
  (let ((destination
	 (make-marray destclass :dimensions (array-dimensions source))))
    (copy-to-destination source destination)))

;;; Copy to a named marray element-type, where the type is a list
(defmethod copy-to-destination ((source array) (destclass list))
  (let ((destination
	 (make-marray destclass :dimensions (array-dimensions source))))
    (copy-to-destination source destination)))

;;; Examples and tests

(generate-all-array-tests vector-copy-to-cl-and-back t
 (cl-array (copy (copy (array-default 3) 'array) 'default-element-type)))

(generate-all-array-tests matrix-copy-to-cl-and-back t
 (cl-array (copy (copy (array-default '(3 3)) 'array) 'default-element-type)))


