;; Vectors
;; Liam Healy 2008-04-13 09:39:02EDT vector-ffa.lisp
;; Time-stamp: <2008-04-27 09:00:46EDT vector-ffa.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Vector structure, CL object, and allocation
;;;;****************************************************************************

;;; GSL-vector definition
(cffi:defcstruct gsl-vector-c
  (size size)
  (stride size)
  (data :pointer)
  (block :pointer)
  (owner :int))

(defclass mvector (gsl-data)
  ()
  (:documentation "GSL vectors."))

;;; Define all the mvector subclasses that are supported by FFA
#.(data-defclass 'vector 'mvector)

;;;;****************************************************************************
;;;; Function definitions
;;;;****************************************************************************

(defmfun set-basis ((object vector) index)
  ("gsl_" :category :type "_set_basis")
  (((mpointer object) :pointer) (index size))
  :definition :generic
  :inputs (object)
  :outputs (object)
  :documentation			; FDL
  "Set the index element to 1, and the rest to 0.")

;;;;;;;;;;;;;; TEST

#|
(letm ((a (vector-double-float x)))
   blah)


(letm ((a (vector-double-float x t))
       (b (vector-double-float #(3 4 5 1 2))))
  (blah blah) blah)

(letm ((a (vector-signed-byte-16 #(1 2 3 4)))
       (b (vector-signed-byte-16 #(3 4 5 1))))
  (m+ a b))


;;; Example with double-float
(let ((a-array
       (make-array*
	'(3)
	'double-float :initial-contents #(-3.21d0 1.0d0 12.8d0))))
  (letm ((a (vector-double-float a-array)))
    (set-all a 111.3d0)
    (list (maref a 0) (maref a 1) (maref a 2))))

;;; Example with (unsigned-byte 8)
(let ((a-array
       (make-array*
	'(3)
	'(unsigned-byte 8) :initial-contents #(5 6 4))))
  (letm ((a (vector-unsigned-byte-8 a-array)))
    (set-all a 1)
    (list (maref a 0) (maref a 1) (maref a 2))))
|#


