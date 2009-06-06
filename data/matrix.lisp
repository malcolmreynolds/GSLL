;; Matrices
;; Liam Healy 2008-04-15 21:57:52EDT matrix.lisp
;; Time-stamp: <2009-06-06 10:07:26EDT matrix.lisp>

(in-package :gsl)

;;; /usr/include/gsl/gsl_matrix_double.h

;;;;****************************************************************************
;;;; Matrix structure and CL object
;;;;****************************************************************************

(export 'matrix)
(defclass matrix (marray)
  ()
  (:documentation "GSL matrices."))

;;; Define all supported matrix subclasses
#.(data-defclass 'matrix 'matrix)

(defmethod contents-from-pointer
    (pointer (struct-type (eql 'gsl-matrix-c))
     &optional (element-type 'double-float))
  (let ((dim0 (cffi:foreign-slot-value pointer struct-type 'size0))
	(dim1 (cffi:foreign-slot-value pointer struct-type 'size1)))
    ;; Copy over from the C side
    (loop for i below dim0
       collect (loop for j below dim1
		  collect (maref pointer i j element-type)))))

(defmethod copy-to-destination
    ((object matrix) (pointer #.+foreign-pointer-class+))
  (foreign-pointer-method
   pointer
   (loop for i below (dim0 object)
      do (loop for j below (dim1 object)
	    do
	    (setf (maref pointer i j (element-type object))
		  (maref object i j))))))

;;;;****************************************************************************
;;;; Mathematical
;;;;****************************************************************************

(defmfun set-identity ((matrix matrix))
  ("gsl_matrix" :type "_set_identity")
  (((mpointer matrix) :pointer))
  :definition :generic
  :c-return :void
  :outputs (matrix)
  :return (matrix)
  :documentation			; FDL
  "Set the elements of the matrix to the
  corresponding elements of the identity matrix, m(i,j) =
  \delta(i,j), i.e. a unit diagonal with all off-diagonal elements zero.
  This applies to both square and rectangular matrices.")

;;;;****************************************************************************
;;;; Copying rows and columns
;;;;****************************************************************************

(defmfun row
    ((matrix matrix) i
     &optional (vector (make-marray element-type :dimensions (dim1 matrix))))
  ("gsl_matrix" :type "_get_row")
  (((mpointer vector) :pointer) ((mpointer matrix) :pointer) (i sizet))
  :definition :generic
  :inputs (matrix)
  :outputs (vector)
  :return (vector)
  :documentation			; FDL
  "Copy the elements of the ith row of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the row.")

(defmfun (setf row) ((vector vector) (matrix matrix) i)
  ("gsl_matrix" :type "_set_row")
  (((mpointer matrix) :pointer) (i sizet) ((mpointer vector) :pointer))
  :definition :generic
  :inputs (vector matrix)
  :outputs (matrix)
  :return (vector)			;setf should return the quantity set
  :documentation			; FDL
  "Copy the elements of the vector into the jth row of the matrix.
  The length of the vector must be the same as the length of the row.")

(defmfun column
    ((matrix matrix) i
     &optional (vector (make-marray element-type :dimensions (dim0 matrix))))
  ("gsl_matrix" :type "_get_col")
  (((mpointer vector) :pointer) ((mpointer matrix) :pointer) (i sizet))
  :definition :generic
  :inputs (matrix)
  :outputs (vector)
  :return (vector)
  :documentation			; FDL
  "Copy the elements of the ith column of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the column.")

(defmfun (setf column) ((vector vector) (matrix matrix) i)
  ("gsl_matrix" :type "_set_col")
  (((mpointer matrix) :pointer) (i sizet) ((mpointer vector) :pointer))
  :definition :generic
  :inputs (vector matrix)
  :outputs (matrix)
  :return (vector)			;setf should return the quantity set
  :documentation			; FDL
  "Copy the elements of the vector into the ith column of the matrix.
  The length of the vector must be the same as the length of the column.")

;;;;****************************************************************************
;;;; Exchanging rows and columns
;;;;****************************************************************************

(defmfun swap-rows ((matrix matrix) i j)
  ("gsl_matrix" :type "_swap_rows")
  (((mpointer matrix) :pointer) (i sizet) (j sizet))
  :definition :generic
  :return (matrix)
  :inputs (matrix)
  :outputs (matrix)
  :documentation 			; FDL
  "Exchange the ith and jth rows of the matrix in-place.")

(defmfun swap-columns ((matrix matrix) i j)
  ("gsl_matrix" :type "_swap_columns")
  (((mpointer matrix) :pointer) (i sizet) (j sizet))
  :definition :generic
  :return (matrix)
  :inputs (matrix)
  :outputs (matrix)
  :documentation 			; FDL
  "Exchange the ith and jth columns of the matrix in-place.")

(defmfun swap-row-column ((matrix matrix) i j)
  ("gsl_matrix" :type "_swap_rowcol")
  (((mpointer matrix) :pointer) (i sizet) (j sizet))
  :definition :generic
  :return (matrix)
  :inputs (matrix)
  :outputs (matrix)
  :documentation 			; FDL
  "Exchange the ith row and jth column of the
   matrix in-place.  The matrix must be square for this operation to
   be possible.")

(defmfun matrix-transpose* ((matrix matrix))
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

(defmfun matrix-transpose
    ((source matrix)
     &optional
     (destination
      (make-marray element-type :dimensions (reverse (dimensions source)))))
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
