;; Matrices
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2008-02-17 09:45:48EST matrix.lisp>
;; $Id: $

(in-package :gsl)

;;; Matrices are specified in a letm binding with
;;;  (matrix-double size-or-initial &optional zero)
;;;  (matrix-single size-or-initial &optional zero)
;;;  (matrix-fixnum size-or-initial &optional zero)
;;;  (matrix-complex size-or-initial &optional zero)
;;; where size-or-initial is a length-2 list of positive integers
;;; indicating the dimensions, and zero indicates that all elements should
;;; be set to zero, or, size-or-initial is a 2D array to which the
;;; vector should be initially set.

;;;;****************************************************************************
;;;; Matrix object definition, allocation, reading & writing
;;;;****************************************************************************

;;; GSL-matrix definition
(cffi:defcstruct gsl-matrix-c
  (size1 size)
  (size2 size)
  (tda size)
  (data :pointer)
  (block :pointer)
  (owner :int))

(defclass gsl-matrix (gsl-data) ())

;;; Allocation, freeing, reading and writing
(defdata "matrix" matrix-double double-float gsl-matrix 2)
(defdata "matrix_float" matrix-single single-float gsl-matrix 2)
(defdata "matrix_int" matrix-fixnum fixnum gsl-matrix 2)
(defdata "matrix_complex" matrix-complex complex gsl-matrix 2)

(defmacro defmfun-mdsfc (&rest args)
  "A defmfun for matrices of double, single, fixnum, and complex."
  (defmfun-all
      '(double single fixnum complex)
      '(:double :float :int gsl-complex)
    "matrix"
    'gsl-matrix
    args))

(defmacro defmfun-mdsf (&rest args)
  "A defmfun for matrices of double, single, and fixnum."
  (defmfun-all
      '(double single fixnum)
      '(:double :float :int)
    "matrix"
    'gsl-matrix
    args))

(defmethod gsl-array ((object gsl-matrix))
  (foreign-slot-value (pointer object) 'gsl-matrix-c 'data))

(export 'matrix-data)
(defun matrix-data (pointer)
  "A pointer to the GSL array with the data contents, from the
   sruct pointer."
  (cffi:foreign-slot-value pointer 'gsl-matrix-c 'data))

;;;;****************************************************************************
;;;; Getting values
;;;;****************************************************************************

(defmfun-mdsfc gsl-aref ((matrix gsl-matrix) &rest indices)
  "gsl_matrix_get"
  (((pointer matrix) :pointer)
   ((first indices) size)
   ((second indices) size))
  :c-return :c-base-type
  :documentation			; FDL
  "The (i,j)-th element of the matrix.")

(defmfun mref (pointer index0 index1)
  "gsl_matrix_get"
  ((pointer :pointer) (index0 size) (index1 size))
  :c-return :double
  :index nil
  :documentation			; FDL
  "An element of the matrix of doubles, computed from the pointer.")

(export 'gsl-matrix-ptr)
(defgeneric gsl-matrix-ptr (matrix i j)
  (:documentation
   "A pointer to the i,j-th element of a matrix."))

(defmfun-mdsfc gsl-matrix-ptr ((matrix gsl-matrix) i j)
  "gsl_matrix_ptr" (((pointer matrix) :pointer) (i size) (j size))
  :c-return :pointer)

(defmethod data ((object gsl-matrix) &optional array)
  (let ((arr (or array
		 (make-array (storage-size object)
			     :element-type (cl-base-type object)))))
    (loop for i from 0
       below (min (array-dimension arr 0) (first (storage-size object)))
       do
       (loop for j from 0
	  below
	  (min (array-dimension arr 1) (second (storage-size object)))
	  do
	  (setf (aref arr i j) (gsl-aref object i j))))
    arr))

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defmfun-mdsfc (setf gsl-aref) (value (matrix gsl-matrix) &rest indices)
  "gsl_matrix_set"
  (((pointer matrix) :pointer)
   ((first indices) size)
   ((second indices) size)
   (value :c-base-type))
  :c-return :void
  :documentation			; FDL
  "Set the (i,j)-th element of the matrix.")

(defmfun (setf mref) (value pointer index0 index1)
  "gsl_matrix_set"
  ((pointer :pointer) (index0 size) (index1 size) (value :double))
  :c-return :void
  :index nil
  :documentation			; FDL
  "Set an element of the matrix of doubles, using its pointer.")

(defmethod (setf data) (array (object gsl-matrix))
  (loop for i from 0
	below
	(min (array-dimension array 0) (first (storage-size object)))
	do
	(loop for j from 0
	      below
	      (min (array-dimension array 1) (second (storage-size object)))
	      do
	      (setf (gsl-aref object i j) (aref array i j)))))

(defmfun-mdsfc set-all ((object gsl-matrix) value)
  "gsl_matrix_set_all"
  (((pointer object) :pointer) (value :c-base-type))
  :c-return :void)

(defmfun-mdsfc set-zero ((object gsl-matrix))
  "gsl_matrix_set_zero"
  (((pointer object) :pointer))
  :c-return :void)

(defmfun-mdsfc set-identity ((matrix gsl-matrix))
  "gsl_matrix_set_identity" (((pointer matrix) gsl-matrix-c))
  :c-return :void
  :documentation			; FDL
  "Set the elements of the matrix m to the
  corresponding elements of the identity matrix, m(i,j) =
  \delta(i,j), i.e. a unit diagonal with all off-diagonal elements zero.
  This applies to both square and rectangular matrices.")

;;;;****************************************************************************
;;;; Matrix Views
;;;;****************************************************************************

(cffi:defcstruct gsl-matrix-view
  (matrix gsl-matrix-c))

(export 'submatrix)
(defgeneric submatrix (matrix k1 k2 n1 n2)
  (:documentation			; FDL
   "A matrix view of a submatrix of the matrix.
   The upper-left element of the submatrix is the element
   (k1, k2) of the original matrix.  The submatrix has n1
   rows and n2 columns.  The physical number of columns in memory
   is unchanged."))

(defmfun-mdsfc submatrix ((matrix gsl-matrix) k1 k2 n1 n2)
  "gsl_matrix_submatrix"
  (((pointer matrix) gsl-matrix-c) (k1 size) (k2 size) (n1 size) (n2 size))
  :c-return gsl-matrix-view)

(export 'matrix-array)
(defgeneric matrix-array (matrix n1 n2)
  (:documentation			; FDL
   "A matrix view of the array.  The
  matrix has n1 rows and n2 columns.  The physical number of
  columns in memory is also given by n2."))

(defmfun-mdsfc matrix-array ((matrix gsl-matrix) n1 n2)
  "gsl_matrix_view_array"
  (((pointer matrix) gsl-matrix-c) (n1 size) (n2 size))
  :c-return gsl-matrix-view)

(export 'matrix-array-tda)
(defgeneric matrix-array-tda (matrix i j tda)
  (:documentation			; FDL
   "A matrix view of the array with a
  physical number of columns tda which may differ from the corresponding
  dimension of the matrix.  The matrix has n1 rows and n2
  columns, and the physical number of columns in memory is given by
  tda."))

(defmfun-mdsfc matrix-array-tda ((matrix gsl-matrix) n1 n2 tda)
  "gsl_matrix_view_array_with_tda"
  (((pointer matrix) gsl-matrix-c) (n1 size) (n2 size) (tda size))
  :c-return gsl-matrix-view)

(defmacro defmfun-mvdsfc (&rest args)
  "A defmfun for vectors of double, single, fixnum, and complex,
  translating to a GSL function named matrix_*."
  (defmfun-all
      '(double single fixnum complex)
      '(:double :float :int gsl-complex)
    "matrix"
    'gsl-vector
    args))

(export 'matrix-vector)
(defgeneric matrix-vector (vector n1 n2)
  (:documentation			; FDL
   "A matrix view of the vector.  The matrix
  has n1 rows and n2 columns. The vector must have unit
  stride. The physical number of columns in memory is also given by
  n2.  Mathematically, the (i,j)-th element of the new
  matrix is given by m'(i,j) = v->data[i*n2 + j]
  where the index i runs from 0 to n1-1 and the index j
  runs from 0 to n2-1.
  The new matrix is only a view of the vector.  When the view
  goes out of scope the original vector will continue to exist.
  The original memory can only be deallocated by freeing the original
  vector.  Of course, the original vector should not be deallocated while
  the view is still in use."))

(defmfun-mvdsfc matrix-vector ((v gsl-vector) n1 n2)
  "gsl_matrix_view_vector"
  (((pointer v) gsl-vector-c) (n1 size) (n2 size))
  :c-return gsl-matrix-view)

(export 'matrix-vector-tda)
(defgeneric matrix-vector-tda (vector n1 n2 tda)
  (:documentation			; FDL
  "A matrix view of the vector with a
  physical number of columns tda which may differ from the
  corresponding matrix dimension.  The vector must have unit stride. The
  matrix has n1 rows and n2 columns, and the physical number
  of columns in memory is given by tda.  Mathematically, the
  (i,j)-th element of the new matrix is given by
  m'(i,j) = v->data[i*tda + j]
  where the index i runs from 0 to n1-1 and the index j
  runs from 0 to n2-1.
  The new matrix is only a view of the vector.  When the view
  goes out of scope the original vector will continue to exist.
  The original memory can only be deallocated by freeing the original
  vector.  Of course, the original vector should not be deallocated while
  the view is still in use."))

(defmfun-mvdsfc matrix-vector-tda ((v gsl-vector) n1 n2 tda)
    "gsl_matrix_view_vector_with_tda"
  (((pointer v) gsl-vector-c) (n1 size) (n2 size) (tda size))
  :c-return gsl-matrix-view)

;;;;****************************************************************************
;;;; Row and Column Views
;;;;****************************************************************************

(export 'row-view)
(defgeneric row-view (matrix i)
  (:documentation			; FDL
   "A vector view of the ith row of the matrix."))

(defmfun-mdsfc row-view ((matrix gsl-matrix) i) 
  "gsl_matrix_row" (((pointer matrix) gsl-matrix-c) (i size))
  :c-return gsl-vector-view
  :null-pointer-info (:EFAULT (format nil "index ~d out of range" i)))

(export 'column-view)
(defgeneric column-view (matrix j)
  (:documentation			; FDL
   "A vector view of the jth column of the matrix."))

(defmfun-mdsfc column-view ((matrix gsl-matrix) j)
  "gsl_matrix_column" ((matrix gsl-matrix-c) (j size))
  :c-return gsl-matrix-view
  :null-pointer-info (:EFAULT (format nil "index ~d out of range" j)))

(export 'diagonal-view)
(defgeneric diagonal-view (matrix)
  (:documentation			; FDL
   "A vector view of the diagonal of the matrix.
   The matrix is not required to be square.
   For a rectangular matrix the length of the diagonal is the same as the smaller
   dimension of the matrix."))

(defmfun-mdsfc diagonal-view ((matrix gsl-matrix))
  "gsl_matrix_diagonal" ((matrix gsl-matrix-c))
  :c-return gsl-matrix-view)

(export 'subdiagonal-view)
(defgeneric subdiagonal-view (matrix k)
  (:documentation			; FDL
   "A vector view of the kth subdiagonal of the matrix; it is not
   required to be square.  The diagonal of the matrix corresponds to
   k = 0."))

(defmfun-mdsfc subdiagonal-view ((matrix gsl-matrix) k)
  "gsl_matrix_subdiagonal" ((matrix gsl-matrix-c) (k size))
  :c-return gsl-matrix-view)

(export 'superdiagonal-view)
(defgeneric superdiagonal-view (matrix k)
  (:documentation			; FDL
  "A vector view of the kth superdiagonal of the matrix; it is not
   required to be square. The diagonal of the matrix corresponds to k = 0."))

(defmfun-mdsfc superdiagonal-view ((matrix gsl-matrix) k)
  "gsl_matrix_superdiagonal"
  ((matrix gsl-matrix-c) (k size))
  :c-return gsl-matrix-view)

;;;;****************************************************************************
;;;; Copying
;;;;****************************************************************************

(defmfun-mdsfc copy ((destination gsl-matrix) (source gsl-matrix))
  "gsl_matrix_memcpy"
  (((pointer destination) gsl-matrix-c) ((pointer source) gsl-matrix-c))
  :invalidate (destination)
  :documentation			; FDL
  "Copy the elements of the matrix source into the
   matrix destination.  The two matrices must have the same size.")

(defmfun-mdsfc swap ((m1 gsl-matrix) (m2 gsl-matrix))
  "gsl_matrix_swap"
  (((pointer m1) gsl-matrix-c) ((pointer m2) gsl-matrix-c))
  :invalidate (m1 m2)
  :documentation			; FDL
  "Exchange the elements of the matrices m1 and
   m2 by copying.  The two matrices must have the same size.")

;;;;****************************************************************************
;;;; Copying rows and columns
;;;;****************************************************************************

(export 'row)
(defgeneric row (vector matrix i)
  (:documentation			; FDL
   "Copy the elements of the ith row of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the row."))

(defmfun-mdsfc row ((vector gsl-vector) (matrix gsl-matrix) i)
  "gsl_matrix_get_row"
  (((pointer vector) gsl-vector-c) ((pointer matrix) gsl-matrix-c) (i size))
  :invalidate (vector))

(export 'column)
(defgeneric column (vector matrix j)
  (:documentation			; FDL
  "Copy the elements of the jth column of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the column."))

(defmfun-mdsfc column ((vector gsl-vector) (matrix gsl-matrix) j)
  "gsl_matrix_get_col"
  (((pointer vector) gsl-vector-c) ((pointer matrix) gsl-matrix-c) (j size))
  :invalidate (vector))

(export 'set-row)
(defgeneric set-row (vector matrix i)
  (:documentation			; FDL
  "Copy the elements of the vector into the
   ith row of the matrix.  The length of the vector must be
   the same as the length of the row."))

(defmfun-mdsfc set-row ((matrix gsl-matrix) i (vector gsl-vector))
  "gsl_matrix_set_row"
  (((pointer matrix) gsl-matrix-c) (i size) ((pointer vector) gsl-vector-c))
  :invalidate (matrix))

(export 'set-column)
(defgeneric set-column (vector matrix j)
  (:documentation 			; FDL
   "Copy the elements of the vector into the jth column of the matrix.
  The length of the vector must be the same as the length of the column."))

(defmfun-mdsfc set-column ((matrix gsl-matrix) j (vector gsl-vector))
  "gsl_matrix_set_col"
  (((pointer matrix) gsl-matrix-c) (j size) ((pointer vector) gsl-vector-c))
  :invalidate (matrix))

(defun (setf row) (vector matrix i) (set-row matrix i vector))
(defun (setf column) (vector matrix j) (set-column matrix j vector))

;;;;****************************************************************************
;;;; Exchanging rows and columns
;;;;****************************************************************************

(export 'swap-rows)
(defgeneric swap-rows (matrix i j)
  (:documentation 			; FDL
  "Exchange the ith and jth rows of the matrix in-place."))

(defmfun-mdsfc swap-rows ((matrix gsl-matrix) i j)
  "gsl_matrix_swap_rows"
  (((pointer matrix) gsl-matrix-c) (i size) (j size))
  :invalidate (matrix))

(export 'swap-columns)
(defgeneric swap-columns (matrix i j)
  (:documentation 			; FDL
  "Exchange the ith and jth columns of the matrix in-place."))

(defmfun-mdsfc swap-columns ((matrix gsl-matrix) i j)
  "gsl_matrix_swap_columns"
  (((pointer matrix) gsl-matrix-c) (i size) (j size))
  :invalidate (matrix))

(export 'swap-rowcol)
(defgeneric swap-rowcol (matrix i j)
  (:documentation			; FDL
  "Exchange the ith row and jth column of the
   matrix in-place.  The matrix must be square for this operation to
   be possible."))

(defmfun-mdsfc swap-rowcol ((matrix gsl-matrix) i j)
  "gsl_matrix_swap_rowcol"
  (((pointer matrix) gsl-matrix-c) (i size) (j size))
  :invalidate (matrix))

(export 'matrix-transpose-copy)
(defgeneric matrix-transpose-copy (destination source)
  (:documentation			; FDL
   "Make the destination matrix the transpose of the source matrix
   by copying the elements.  The dimensions of the destination
   matrix must match the transposed dimensions of the source."))

(defmfun-mdsfc matrix-transpose-copy
    ((destination gsl-matrix) (source gsl-matrix))
  "gsl_matrix_transpose_memcpy"
  (((pointer destination) gsl-matrix-c) ((pointer source) gsl-matrix-c))
  :invalidate (destination))

(export 'matrix-transpose)
(defgeneric matrix-transpose (matrix)
  (:documentation			; FDL
   "Replace the matrix by its transpose by copying the elements
   of the matrix in-place.  The matrix must be square for this
   operation to be possible."))

(defmfun-mdsfc matrix-transpose ((matrix gsl-matrix))
  "gsl_matrix_transpose"
  (((pointer matrix) gsl-matrix-c))
  :invalidate (matrix))

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(defmfun-mdsfc gsl+ ((a gsl-matrix) (b gsl-matrix))
    "gsl_matrix_add"
  (((pointer a) gsl-matrix-c) ((pointer b) gsl-matrix-c))
  :invalidate (a)
  :documentation			; FDL
  "Add the elements of b to the elements of a,
   a'_i = a_i + b_i. The two matrices must have the
   same dimensions.")

(defmfun-mdsfc gsl- ((a gsl-matrix) (b gsl-matrix))
  "gsl_matrix_sub" (((pointer a) gsl-matrix-c) ((pointer b) gsl-matrix-c))
  :invalidate (a)
  :documentation			; FDL
  "Subtract the elements of matrix b from the elements of matrix
   a, a'_i = a_i - b_i. The two matrices must have the
   same dimensions.")

(defmfun-mdsfc gsl* ((a gsl-matrix) (b gsl-matrix))
  "gsl_matrix_mul_elements"
  (((pointer a) gsl-matrix-c) ((pointer b) gsl-matrix-c))
  :invalidate (a)
  :documentation			; FDL
  "Multiply the elements of matrix a by the elements of
  matrix b, a'(i,j) = a(i,j) * b(i,j). The two matrices must have the
  same dimensions.")

(defmfun-mdsfc gsl/ ((a gsl-matrix) (b gsl-matrix))
  "gsl_matrix_div_elements"
  (((pointer a) gsl-matrix-c) ((pointer b) gsl-matrix-c))
  :invalidate (a)
  :documentation			; FDL
  "Divide the elements of matrix a by the elements of
   matrix b, a'(i,j) = a(i,j) / b(i,j). The two matrices must have the
   same dimensions.")

(defmfun-mdsfc gsl*c ((a gsl-matrix) x)
  "gsl_matrix_scale" (((pointer a) gsl-matrix-c) (x :c-base-type))
  :invalidate (a)
  :documentation			; FDL
  "Multiply the elements of matrix a by the constant
  factor x, a'(i,j) = x a(i,j).")

(defmfun-mdsfc gsl+c ((a gsl-matrix) x)
  "gsl_matrix_add_constant" (((pointer a) gsl-matrix-c) (x :c-base-type))
  :invalidate (a)
  :documentation			; FDL
  "Add the constant value x to the elements of the
  matrix a, a'(i,j) = a(i,j) + x.")

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(defmfun-mdsf gsl-max ((m gsl-matrix))
  "gsl_matrix_max" (((pointer m) gsl-matrix-c))
  :documentation			; FDL
  "The maximum value in the matrix m."
  :c-return :c-base-type)

(defmfun-mdsf gsl-min ((m gsl-matrix))
  "gsl_matrix_min" (((pointer m) gsl-matrix-c))
  :documentation			; FDL
  "The minimum value in the matrix m."
  :c-return :c-base-type)

(defmfun-mdsf gsl-minmax ((m gsl-matrix))
  "gsl_matrix_minmax"
  (((pointer m) gsl-matrix-c) (min :c-base-type) (max :c-base-type))
  :documentation			; FDL
  "The minimum and maximum values in the matrix m."
  :c-return :void)

(defmfun-mdsf gsl-max-index ((m gsl-matrix))
  "gsl_matrix_max_index"
  (((pointer m) gsl-matrix-c) (imax size) (jmax size))
  :documentation			; FDL
  "The index of the maximum value in the matrix m
   When there are several equal maximum elements then the lowest index is
   returned."
  :c-return :void
  :return ((list (scref imax) (scref jmax))))

(defmfun-mdsf gsl-min-index ((m gsl-matrix))
  "gsl_matrix_min_index"
  (((pointer m) gsl-matrix-c) (imin size) (jmin size))
  :documentation			; FDL
  "The index of the minimum value in the matrix m
  When there are several equal minimum elements then the
  lowest index is returned."
  :c-return :void
  :return ((list (scref imin) (scref jmin))))

(defmfun-mdsf gsl-minmax-index ((m gsl-matrix))
  "gsl_matrix_minmax_index"
  (((pointer m) gsl-matrix-c)
   (imin size) (jmin size) (imax size) (jmax size))
  :documentation			; FDL
  "The indices of the minimum and maximum values in the matrix m.
  When there are several equal minimum elements then the lowest index is
  returned."
  :c-return :void
  :return ((list (scref imin) (scref jmin))
	   (list (scref imax) (scref jmax))))

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

(defmfun-mdsfc gsl-zerop ((m gsl-matrix))
  "gsl_matrix_isnull" ((m gsl-matrix-c))
  :documentation			; FDL
  "All elements of matrix m are zero."
  :c-return :boolean)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests matrix-fixnum
 (letm ((intmat (matrix-fixnum 2 2)))	;(setf gsl-aref), gsl-aref
   (setf (gsl-aref intmat 0 1) 77)
   (gsl-aref intmat 0 1))
 (letm ((intmat (matrix-fixnum 2 2)))	;(setf data)
   (setf (data intmat) #2A((4 6) (8 2)))
   (data intmat))
 (letm ((intmat (matrix-fixnum 2 2)))	;set-zero
   (set-zero intmat)
   (data intmat))
 (letm ((intmat (matrix-fixnum 2 2)))	;set-all
   (set-all intmat 44)
   (data intmat))
 (letm ((intmat (matrix-fixnum 2 2)))	;set-identity
   (set-identity intmat)
   (data intmat))
 (letm ((intmat (matrix-fixnum #2A((4 6) (8 2)))) ;row
	(vect (vector-fixnum 2)))
   (row vect intmat 0)
   (data vect))
 (letm ((intmat (matrix-fixnum #2A((4 6) (8 2)))) ;column
	(vect (vector-fixnum 2)))
   (column vect intmat 1)
   (data vect))
 (letm ((intmat (matrix-fixnum #2A((-1 -12) (8 3))))) ;gsl-min
   (gsl-min intmat))
 (letm ((intmat (matrix-fixnum #2A((-1 -12) (8 3))))) ;gsl-max
   (gsl-max intmat))
 (letm ((intmat (matrix-fixnum #2A((-1 -12) (8 3))))) ;gsl-minmax
   (multiple-value-list (gsl-minmax intmat)))
 (letm ((intmat (matrix-fixnum #2A((-1 -12) (8 3))))) ;gsl-min-index
   (gsl-min-index intmat))
 (letm ((intmat (matrix-fixnum #2A((-1 -12) (8 3))))) ;gsl-max-index
   (gsl-max-index intmat))
 (letm ((intmat (matrix-fixnum #2A((-1 -12) (8 3))))) ;gsl-minmax-index
   (multiple-value-list (gsl-minmax-index intmat)))
 (letm ((intmat1 (matrix-fixnum #2A((1 2)(3 4)))) ;copy
	(intmat2 (matrix-fixnum 2 2)))
   (copy intmat2 intmat1)
   (data intmat2))
 (letm ((intmat1 (matrix-fixnum #2A((1 2)(3 4)))) ;swap
	(intmat2 (matrix-fixnum #2A((5 6) (7 8)))))
   (swap intmat1 intmat2)
   (data intmat1))
 (letm ((intmat1 (matrix-fixnum #2A((1 2)(3 4))))) ;swap-rows
   (swap-rows intmat1 0 1)
   (data intmat1))
 (letm ((intmat1 (matrix-fixnum #2A((1 2)(3 4))))) ;swap-columns
   (swap-columns intmat1 0 1)
   (data intmat1))
 (letm ((intmat1 (matrix-fixnum #2A((1 2)(3 4))))) ;swap-rowcol
   (swap-rowcol intmat1 0 1)
   (data intmat1)))
|#

(LISP-UNIT:DEFINE-TEST
    MATRIX-FIXNUM
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 77)
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM 2 2)))
      (SETF (GSL-AREF INTMAT 0 1) 77)
      (GSL-AREF INTMAT 0 1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((4 6) (8 2)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM 2 2)))
      (SETF (DATA INTMAT) #2A((4 6) (8 2)))
      (DATA INTMAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((0 0) (0 0)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM 2 2))) (SET-ZERO INTMAT)
	  (DATA INTMAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((44 44) (44 44)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM 2 2)))
      (SET-ALL INTMAT 44) (DATA INTMAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((1 0) (0 1)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM 2 2)))
      (SET-IDENTITY INTMAT) (DATA INTMAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(4 6))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM #2A((4 6) (8 2))))
	 (VECT (VECTOR-FIXNUM 2)))
      (ROW VECT INTMAT 0)
      (DATA VECT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(6 2))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM #2A((4 6) (8 2))))
	 (VECT (VECTOR-FIXNUM 2)))
      (COLUMN VECT INTMAT 1)
      (DATA VECT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -12)
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM #2A((-1 -12) (8 3)))))
      (GSL-MIN INTMAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 8)
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM #2A((-1 -12) (8 3)))))
      (GSL-MAX INTMAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST -12 8))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM #2A((-1 -12) (8 3)))))
      (MULTIPLE-VALUE-LIST (GSL-MINMAX INTMAT)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 0 1))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM #2A((-1 -12) (8 3)))))
      (GSL-MIN-INDEX INTMAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 1 0))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM #2A((-1 -12) (8 3)))))
      (GSL-MAX-INDEX INTMAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST (LIST 0 1) (LIST 1 0)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT (MATRIX-FIXNUM #2A((-1 -12) (8 3)))))
      (MULTIPLE-VALUE-LIST
       (GSL-MINMAX-INDEX INTMAT)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((1 2) (3 4)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT1 (MATRIX-FIXNUM #2A((1 2) (3 4))))
	   (INTMAT2 (MATRIX-FIXNUM 2 2)))
      (COPY INTMAT2 INTMAT1) (DATA INTMAT2))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((5 6) (7 8)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT1 (MATRIX-FIXNUM #2A((1 2) (3 4))))
	   (INTMAT2 (MATRIX-FIXNUM #2A((5 6) (7 8)))))
      (SWAP INTMAT1 INTMAT2) (DATA INTMAT1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((3 4) (1 2)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT1 (MATRIX-FIXNUM #2A((1 2) (3 4)))))
      (SWAP-ROWS INTMAT1 0 1) (DATA INTMAT1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((2 1) (4 3)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT1 (MATRIX-FIXNUM #2A((1 2) (3 4)))))
      (SWAP-COLUMNS INTMAT1 0 1) (DATA INTMAT1))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((2 4) (3 1)))
   (MULTIPLE-VALUE-LIST
    (LETM ((INTMAT1 (MATRIX-FIXNUM #2A((1 2) (3 4)))))
      (SWAP-ROWCOL INTMAT1 0 1) (DATA INTMAT1)))))


#|
(make-tests
 matrix-double
 (letm ((mat (matrix-double 10 3)))
   (loop for i from 0 below 10
	 do
	 (loop for j from 0 below 3
	       do (setf (gsl-aref mat i j) (+ 0.23d0 j (* 100 i)))))
   (data mat))
 (letm ((mat (matrix-double #2A((1.0d0 2.0d0) (3.0d0 4.0d0))))
	(ans (matrix-double 2 2)))
   (copy ans mat)
   (data ans))
 (letm ((mat (matrix-double #2A((1.0d0 2.0d0) (3.0d0 4.0d0)))))
   (gsl* mat mat)
   (data mat)))
|#

(LISP-UNIT:DEFINE-TEST MATRIX-DOUBLE
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((0.23d0 1.23d0 2.23d0)
	(100.23d0 101.23d0 102.23d0)
	(200.23d0 201.23d0 202.23d0)
	(300.23d0 301.23d0 302.23d0)
	(400.23d0 401.23d0 402.23d0)
	(500.23d0 501.23d0 502.23d0)
	(600.23d0 601.23d0 602.23d0)
	(700.23d0 701.23d0 702.23d0)
	(800.23d0 801.23d0 802.23d0)
	(900.23d0 901.23d0 902.23d0)))
   (MULTIPLE-VALUE-LIST
    (LETM ((MAT (MATRIX-DOUBLE 10 3)))
      (LOOP FOR I FROM 0 BELOW 10 DO
	    (LOOP FOR J FROM 0 BELOW 3 DO
		  (SETF (GSL-AREF MAT I J)
			(+ 0.23d0 J (* 100 I)))))
      (DATA MAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((1.0d0 2.0d0) (3.0d0 4.0d0)))
   (MULTIPLE-VALUE-LIST
    (LETM ((MAT (MATRIX-DOUBLE #2A((1.0d0 2.0d0) (3.0d0 4.0d0))))
	 (ANS (MATRIX-DOUBLE 2 2)))
      (COPY ANS MAT) (DATA ANS))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((1.0d0 4.0d0) (9.0d0 16.0d0)))
   (MULTIPLE-VALUE-LIST
    (LETM ((MAT (MATRIX-DOUBLE #2A((1.0d0 2.0d0) (3.0d0 4.0d0)))))
      (GSL* MAT MAT) (DATA MAT)))))

