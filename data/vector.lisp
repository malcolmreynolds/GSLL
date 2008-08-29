;; Vectors
;; Liam Healy 2008-04-13 09:39:02EDT vector-ffa.lisp
;; Time-stamp: <2008-08-28 21:41:15EDT vector.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Vector structure, CL object, and allocation
;;;;****************************************************************************

;;; GSL-vector definition
(cffi:defcstruct gsl-vector-c
  (size sizet)
  (stride sizet)
  (data :pointer)
  (block :pointer)
  (owner :int))

(defclass mvector (gsl-data)
  ()
  (:documentation "GSL vectors."))

;;; Define all the mvector subclasses that are supported by FFA
#.(data-defclass 'vector 'mvector)

(defun make-vector-from-pointer ())

;;;;****************************************************************************
;;;; Function definitions
;;;;****************************************************************************

(defmfun set-basis ((object vector) index)
  ("gsl_" :category :type "_set_basis")
  (((mpointer object) :pointer) (index sizet))
  :definition :generic
  :inputs (object)
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
  (((mpointer vec) gsl-vector-c))
  :definition :generic
  :inputs (vec)
  :outputs (vec)
  :return (vec)
  :documentation			; FDL
  "Reverse the order of the elements of the vector vec.")

;;;;****************************************************************************
;;;; Examples and unit tests
;;;;****************************************************************************

#|
(let ((ac (make-array* 3 'double-float :initial-contents '(2.0d0 -23.1d0 55.0d0))))
  (letm ((a (vector-double-float ac t))
	 (b (vector-double-float 3)))
    (set-all b 12.0d0)
    (m+ a b))
  ac)

|#
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


#|
(make-tests
 vector-signed-byte-32
 (letm ((intvec (vector-signed-byte-32 4)))	;(setf maref), maref
   (setf (maref intvec 1) 77)
   (maref intvec 1))
 (letm ((intvec (vector-signed-byte-32 4)))	;(setf data)
   (setf (data intvec) #(4 6 8 2))
   (data intvec))
 (letm ((intvec (vector-signed-byte-32 4)))	;set-zero
   (set-zero intvec)
   (data intvec))
 (letm ((intvec (vector-signed-byte-32 4)))	;set-all
   (set-all intvec 44)
   (data intvec))
 (letm ((intvec (vector-signed-byte-32 4)))	;set-basis
   (set-basis intvec 1)
   (data intvec))
 (letm ((intvec (vector-signed-byte-32 #(1 2 3 4)))) ;vector-reverse
   (vector-reverse intvec)
   (data intvec))
 (letm ((intvec (vector-signed-byte-32 #(-1 -12 8 3))))	;gsl-min
   (gsl-min intvec))
 (letm ((intvec (vector-signed-byte-32 #(-1 -12 8 3))))	;gsl-max
   (gsl-max intvec))
 (letm ((intvec (vector-signed-byte-32 #(-1 -12 8 3))))	;gsl-minmax
   (multiple-value-list (gsl-minmax intvec)))
 (letm ((intvec (vector-signed-byte-32 #(-1 -12 8 3))))	;gsl-min-index
   (gsl-min-index intvec))
 (letm ((intvec (vector-signed-byte-32 #(-1 -12 8 3))))	;gsl-max-index
   (gsl-max-index intvec))
 (letm ((intvec (vector-signed-byte-32 #(-1 -12 8 3))))	;gsl-minmax-index
   (multiple-value-list (gsl-minmax-index intvec)))
 (letm ((intvec1 (vector-signed-byte-32 #(1 2 3 4))) ;copy
	(intvec2 (vector-signed-byte-32 4)))
   (copy intvec2 intvec1)
   (data intvec2))
 (letm ((intvec1 (vector-signed-byte-32 #(1 2 3 4))) ;swap
	(intvec2 (vector-signed-byte-32 #(5 6 7 8))))
   (swap intvec2 intvec1)
   (concatenate 'vector (data intvec1) (data intvec2)))
 (letm ((intvec (vector-signed-byte-32 #(1 2 3 4)))) ;swap-elements
   (swap-elements intvec 1 3)
   (data intvec)))
|#

#|
(make-tests
 vector-double
 (letm ((vec (vector-double-float 3)))
   (setf (maref vec 0) -3.21d0
	 (maref vec 1) 1.0d0
	 (maref vec 2) 12.8d0
	 (cl-invalid vec) t)
   (data vec))
 (letm ((vec (vector-double-float 3)))
   (setf (data vec) #(-3.21d0 1.0d0 12.8d0))
   (data vec))
 (letm ((vec (vector-double-float #(-3.21d0 1.0d0 12.8d0))))
   (data vec))
 (letm ((base (vector-double-float 5)))
   (set-basis base 1)
   (data base))
 (letm ((vec1 (vector-double-float #(-3.21d0 1.0d0 12.8d0)))
	(vec2 (vector-double-float 3)))
   (copy vec2 vec1)
   (data vec2)))
|#

