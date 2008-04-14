;; Vectors
;; Liam Healy 2008-04-13 09:39:02EDT vector-ffa.lisp
;; Time-stamp: <2008-04-13 23:14:09EDT vector-ffa.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Vector structure and CL object
;;;;****************************************************************************

(defclass mvector (gsl-data)
  ()
  (:documentation "GSL vectors."))

;;; Define all the mvector subclasses that are supported by FFA
#.(data-defclass 'vector 'mvector)

(defmfun alloc-from-block ((object vector))
  "gsl_vector_alloc_from_block"
  (((block-pointer object) :pointer)
   (0 size)
   (totsize size)
   (1 size))
  :category vector
  :global ((totsize (total-size object)))
  :c-return :pointer
  :documentation "Allocate memory for the GSL struct given a block pointer.")
 
(defmfun set-all ((object vector) value)
  "gsl_vector_set_all"
  (((mpointer object) :pointer) (value :c-base-type))
  :category vector
  :outputs (object)
  :c-return :void
  :documentation "Set all elements to the value.")

;;;;;;;;;;;;;; TEST

#|
(letm ((a (vector-double-float x)))
   blah)


(letm ((a (vector-double-float x t))
       (b (vector-double-float #(3 4 5 1 2))))
  (blah blah) blah)



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


