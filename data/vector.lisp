;; Vectors
;; Liam Healy 2008-04-13 09:39:02EDT vector.lisp
;; Time-stamp: <2009-06-06 10:04:13EDT vector.lisp>

(in-package :gsl)

;;; /usr/include/gsl/gsl_vector_double.h

;;;;****************************************************************************
;;;; Vector structure, CL object, and allocation
;;;;****************************************************************************

(export 'mvector)
(defclass mvector (marray)
  ()
  (:documentation "GSL vectors."))

;;; Define all supported mvector subclasses
#.(data-defclass 'vector 'mvector)

(defmethod contents-from-pointer
    (pointer (struct-type (eql 'gsl-vector-c))
     &optional (element-type 'double-float))
  (loop for i below (cffi:foreign-slot-value pointer struct-type 'size)
     collect (maref pointer i nil element-type)))

(defmethod copy-to-destination
    ((object mvector) (pointer #.+foreign-pointer-class+))
  (foreign-pointer-method
   pointer
   (loop for i below (dim0 object)
      do (setf (maref pointer i nil (element-type object))
	       (maref object i)))))

;;;;****************************************************************************
;;;; Function definitions
;;;;****************************************************************************

(defmfun set-basis ((object vector) index)
  ("gsl_" :category :type "_set_basis")
  (((mpointer object) :pointer) (index sizet))
  :definition :generic
  :inputs (object)
  :outputs (object)
  :return (object)
  :outputs (object)
  :documentation			; FDL
  "Set the index element to 1, and the rest to 0.")

(defmfun swap-elements ((vec vector) i j)
  ("gsl_" :category :type "_swap_elements")
  (((mpointer vec) :pointer) (i sizet) (j sizet))
  :definition :generic
  :inputs (vec)
  :outputs (vec)
  :return (vec)
  :documentation			; FDL
  "Exchange the i-th and j-th elements of the vector vec in-place.")

(defmfun vector-reverse ((vec vector))
  ("gsl_" :category :type "_reverse")
  (((mpointer vec) :pointer))
  :definition :generic
  :inputs (vec)
  :outputs (vec)
  :return (vec)
  :documentation			; FDL
  "Reverse the order of the elements of the vector vec.")
