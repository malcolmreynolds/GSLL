;; Matrices
;; Liam Healy 2008-04-15 21:57:52EDT matrix-ffa.lisp
;; Time-stamp: <2008-04-27 18:24:29EDT matrix-ffa.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Matrix structure and CL object
;;;;****************************************************************************

;;; GSL-matrix definition
(cffi:defcstruct gsl-matrix-c
  (size1 size)
  (size2 size)
  (tda size)
  (data :pointer)
  (block :pointer)
  (owner :int))

(defclass matrix (gsl-data)
  ()
  (:documentation "GSL matrices."))

;;; Define all the mvector subclasses that are supported by FFA
#.(data-defclass 'matrix 'matrix)

;;;;****************************************************************************
;;;; Mathematical
;;;;****************************************************************************

(defmfun set-identity ((matrix matrix))
  ("gsl_matrix" :type "_set_identity")
  (((mpointer matrix) :pointer))
  :definition :generic
  :c-return :void
  :return (matrix)
  :documentation			; FDL
  "Set the elements of the matrix to the
  corresponding elements of the identity matrix, m(i,j) =
  \delta(i,j), i.e. a unit diagonal with all off-diagonal elements zero.
  This applies to both square and rectangular matrices.")

;;;;****************************************************************************
;;;; Copying rows and columns
;;;;****************************************************************************

(defmfun row ((vector vector) (matrix matrix) i)
  ("gsl_matrix" :type "_get_row")
  (((mpointer vector) :pointer) ((mpointer matrix) :pointer) (i size))
  :definition :generic
  :return (vector)
  :documentation			; FDL
  "Copy the elements of the ith row of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the row.")

(defmfun (setf row) ((vector vector) (matrix matrix) i)
  ("gsl_matrix" :type "_set_row")
  (((mpointer matrix) :pointer) (i size) ((mpointer vector) :pointer))
  :definition :generic
  :return (vector)			;setf should return
  :documentation			; FDL
  "Copy the elements of the vector into the jth column of the matrix.
  The length of the vector must be the same as the length of the column.")

(defmfun column ((vector vector) (matrix matrix) i)
  ("gsl_matrix" :type "_get_col")
  (((mpointer vector) :pointer) ((mpointer matrix) :pointer) (i size))
  :definition :generic
  :return (vector)
  :documentation			; FDL
  "Copy the elements of the ith column of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the column.")

(defmfun (setf column) ((vector vector) (matrix matrix) i)
  ("gsl_matrix" :type "_set_col")
  (((mpointer matrix) :pointer) (i size) ((mpointer vector) :pointer))
  :definition :generic
  :return (vector)			;setf should return
  :documentation			; FDL
  "Copy the elements of the vector into the ith column of the matrix.
  The length of the vector must be the same as the length of the column.")

;;;;****************************************************************************
;;;; Exchanging rows and columns
;;;;****************************************************************************

(defmfun swap-rows ((matrix matrix) i j)
  ("gsl_matrix" :type "_swap_rows")
  (((mpointer matrix) :pointer) (i size) (j size))
  :definition :generic
  :return (matrix)
  :inputs (matrix)
  :outputs (matrix)
  :documentation 			; FDL
  "Exchange the ith and jth rows of the matrix in-place.")

(defmfun swap-columns ((matrix matrix) i j)
  ("gsl_matrix" :type "_swap_columns")
  (((mpointer matrix) :pointer) (i size) (j size))
  :definition :generic
  :return (matrix)
  :inputs (matrix)
  :outputs (matrix)
  :documentation 			; FDL
  "Exchange the ith and jth columns of the matrix in-place.")

(defmfun swap-row-column ((matrix matrix) i j)
  ("gsl_matrix" :type "_swap_rowcol")
  (((mpointer matrix) :pointer) (i size) (j size))
  :definition :generic
  :return (matrix)
  :inputs (matrix)
  :outputs (matrix)
  :documentation 			; FDL
  "Exchange the ith row and jth column of the
   matrix in-place.  The matrix must be square for this operation to
   be possible.")

(defmfun matrix-transpose ((matrix matrix))
  ("gsl_matrix" :type "_transpose")
  (((mpointer matrix) :pointer))
  :definition :generic
  :return (matrix)
  :inputs (matrix)
  :outputs (matrix)
  :documentation 			; FDL
  "Replace the matrix by its transpose by copying the elements
   of the matrix in-place.  The matrix must be square for this
   operation to be possible.")

(defmfun matrix-transpose-copy ((destination matrix) (source matrix))
  ("gsl_matrix" :type "_transpose_memcpy")
  (((mpointer destination) :pointer) ((mpointer source) :pointer))
  :definition :generic
  :return (destination)
  :inputs (source)
  :outputs (destination)
  :documentation 			; FDL
  "Make the destination matrix the transpose of the source matrix
   by copying the elements.  The dimensions of the destination
   matrix must match the transposed dimensions of the source.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(letm ((mat (matrix-double-float (els (2.0d0 -23.1d0) (55.0d0 -1.0d0)) t)))
  (minmax mat))
|#

#|
(make-tests matrix-signed-byte-32
 (letm ((intmat (matrix-signed-byte-32 2 2)))	;(setf maref), maref
   (setf (maref intmat 0 1) 77)
   (maref intmat 0 1))
 (letm ((intmat (matrix-signed-byte-32 2 2)))	;(setf data)
   (setf (data intmat) #2A((4 6) (8 2)))
   (data intmat))
 (letm ((intmat (matrix-signed-byte-32 2 2)))	;set-zero
   (set-zero intmat)
   (data intmat))
 (letm ((intmat (matrix-signed-byte-32 2 2)))	;set-all
   (set-all intmat 44)
   (data intmat))
 (letm ((intmat (matrix-signed-byte-32 2 2)))	;set-identity
   (set-identity intmat)
   (data intmat))
 (letm ((intmat (matrix-signed-byte-32 #2A((4 6) (8 2)))) ;row
	(vect (vector-signed-byte-32 2)))
   (row vect intmat 0)
   (data vect))
 (letm ((intmat (matrix-signed-byte-32 #2A((4 6) (8 2)))) ;column
	(vect (vector-signed-byte-32 2)))
   (column vect intmat 1)
   (data vect))
 (letm ((intmat (matrix-signed-byte-32 #2A((-1 -12) (8 3))))) ;gsl-min
   (gsl-min intmat))
 (letm ((intmat (matrix-signed-byte-32 #2A((-1 -12) (8 3))))) ;gsl-max
   (gsl-max intmat))
 (letm ((intmat (matrix-signed-byte-32 #2A((-1 -12) (8 3))))) ;gsl-minmax
   (multiple-value-list (gsl-minmax intmat)))
 (letm ((intmat (matrix-signed-byte-32 #2A((-1 -12) (8 3))))) ;gsl-min-index
   (gsl-min-index intmat))
 (letm ((intmat (matrix-signed-byte-32 #2A((-1 -12) (8 3))))) ;gsl-max-index
   (gsl-max-index intmat))
 (letm ((intmat (matrix-signed-byte-32 #2A((-1 -12) (8 3))))) ;gsl-minmax-index
   (multiple-value-list (gsl-minmax-index intmat)))
 (letm ((intmat1 (matrix-signed-byte-32 #2A((1 2)(3 4)))) ;copy
	(intmat2 (matrix-signed-byte-32 2 2)))
   (copy intmat2 intmat1)
   (data intmat2))
 (letm ((intmat1 (matrix-signed-byte-32 #2A((1 2)(3 4)))) ;swap
	(intmat2 (matrix-signed-byte-32 #2A((5 6) (7 8)))))
   (swap intmat1 intmat2)
   (data intmat1))
 (letm ((intmat1 (matrix-signed-byte-32 #2A((1 2)(3 4))))) ;swap-rows
   (swap-rows intmat1 0 1)
   (data intmat1))
 (letm ((intmat1 (matrix-signed-byte-32 #2A((1 2)(3 4))))) ;swap-columns
   (swap-columns intmat1 0 1)
   (data intmat1))
 (letm ((intmat1 (matrix-signed-byte-32 #2A((1 2)(3 4))))) ;swap-rowcol
   (swap-rowcol intmat1 0 1)
   (data intmat1)))
|#

#|
(make-tests
 matrix-double
 (letm ((mat (matrix-double-float 10 3)))
   (loop for i from 0 below 10
	 do
	 (loop for j from 0 below 3
	       do (setf (maref mat i j) (+ 0.23d0 j (* 100 i)))))
   (data mat))
 (letm ((mat (matrix-double-float #2A((1.0d0 2.0d0) (3.0d0 4.0d0))))
	(ans (matrix-double-float 2 2)))
   (copy ans mat)
   (data ans))
 (letm ((mat (matrix-double-float #2A((1.0d0 2.0d0) (3.0d0 4.0d0)))))
   (m* mat mat)
   (data mat))
 (letm ((mat (matrix-double-float #2A((1.0d0 2.0d0) (-3.0d0 4.0d0)))))
   (list (maref mat 0 0) (maref mat 0 1) (maref mat 1 0) (maref mat 1 1))))
|#
