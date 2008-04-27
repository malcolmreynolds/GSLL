;; Using GSL bulk data (vectors, matrices, etc.) storage.
;; Liam Healy, Sun Mar 26 2006 - 16:32
;; Time-stamp: <2008-04-26 22:13:16EDT mathematical.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Setting values from CL
;;;;****************************************************************************

(defgeneric (setf data) (cl-array object)
  (:documentation "Set the values in the object from a CL array.")
  ;; Default method is to read from a sequence
  (:method (sequence (object gsl-data))
    (loop for i from 0
       below (min (length sequence) (apply #'* (storage-size object)))
       do (setf (maref object i) (elt sequence i))))
  (:method :after (source (object gsl-data))
	   (setf (cl-invalid object) t)))

(export 'set-identity)
(defgeneric set-identity (object)
  (:documentation "Set elements to represent the identity.")
  (:method :after ((object gsl-data)) (cl-invalidate object)))

(export 'set-basis)
(defgeneric set-basis (object index)
  (:documentation "Set indexth basis vector."))

(export 'data-valid)
(defgeneric data-valid (object)
  (:documentation "Validate the values in the object."))

;;;;****************************************************************************
;;;; Copying
;;;;****************************************************************************

(export '(copy swap))
(defgeneric copy (destination source)
  (:documentation "Copy from source to destination."))

(defgeneric swap (obj1 obj2)
  (:documentation "Swap contents of obj1 and obj2."))

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(export '(m+ m- m* m/ m*c m+c))

(defgeneric m+ (a b)
  (:documentation "Add."))

(defgeneric m- (a b)
  (:documentation "Subtract."))

(defgeneric m* (a b)
  (:documentation "Multiply."))

(defgeneric m/ (a b)
  (:documentation "Divide."))

(defgeneric m+c (a x)
  (:documentation "Add scalar."))

(defgeneric m*c (a x)
  (:documentation "Multiply by scalar."))

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(export
 '(gsl-max gsl-min gsl-minmax gsl-max-index gsl-min-index gsl-minmax-index))

(defgeneric gsl-min (a)
  (:documentation "Minimum."))

(defgeneric gsl-max (a)
  (:documentation "Maximum."))

(defgeneric gsl-minmax (a)
  (:documentation "Minimum and maximum."))

(defgeneric gsl-min-index (a)
  (:documentation "Index of minimum."))

(defgeneric gsl-max-index (a)
  (:documentation "Index of maximum."))

(defgeneric gsl-minmax-index (a)
  (:documentation "Indices of minimum and maximum."))

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

(export '(gsl-zerop))

(defgeneric gsl-zerop (a)
  (:documentation "Object is zero."))
