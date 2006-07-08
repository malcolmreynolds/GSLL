;********************************************************
; file:        matrix.lisp                        
; description: Matrices
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Fri Jul  7 2006 - 23:17
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Matrix object definition, allocation, reading & writing
;;;;****************************************************************************

;;; GSL-matrix definition
(cffi:defcstruct gsl-matrix-c
  (size1 :size)
  (size2 :size)
  (tda :size)
  (data :pointer)
  (block :pointer)
  (owner :int))

(defclass gsl-matrix (gsl-data) ())

;;; Allocation, freeing, reading and writing
(defdata "matrix" matrix-double double-float gsl-matrix 2)
(defdata "matrix_float" matrix-single single-float gsl-matrix 2)
(defdata "matrix_int" matrix-fixnum fixnum gsl-matrix 2)
(defdata "matrix_complex" matrix-complex complex gsl-matrix 2)

(defmacro defun-gsl-mdsfc (&rest args)
  "A defun-gsl for matrices of double, single, fixnum, and complex."
  (defun-gsl-all
      '(matrix-double matrix-single matrix-fixnum matrix-complex)
      '(:double :float :int gsl-complex)
    "matrix"
    'gsl-matrix
    args))

(defmacro defun-gsl-mdsf (&rest args)
  "A defun-gsl for matrices of double, single, and fixnum."
  (defun-gsl-all
      '(matrix-double matrix-single matrix-fixnum)
      '(:double :float :int)
    "matrix"
    'gsl-matrix
    args))

(defmethod gsl-array ((object gsl-matrix))
  (foreign-slot-value (pointer object) 'gsl-matrix-c 'data))

;;;;****************************************************************************
;;;; Getting values
;;;;****************************************************************************

(defun-gsl-mdsfc gsl-aref ((matrix gsl-matrix) &rest indices)
  "gsl_matrix_get"
  (((pointer matrix) :pointer)
   ((first indices) :size)
   ((second indices) :size))
  :c-return :c-base-type
  :documentation "The (i,j)-th element of the matrix.")

(defun-gsl-mdsfc gsl-matrix-ptr ((matrix gsl-matrix) i j)
  "gsl_matrix_ptr" (((pointer matrix) :pointer) (i :size) (j :size))
  :c-return :pointer
  :documentation "A pointer to the @math{(i,j)}-th element of a
  matrix @var{m}.")

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

(defun-gsl-mdsfc (setf gsl-aref) (value (matrix gsl-matrix) &rest indices)
  "gsl_matrix_set"
  (((pointer matrix) :pointer)
   ((first indices) :size)
   ((second indices) :size)
   (value :c-base-type))
  :c-return :void
  :documentation "Set the (i,j)-th element of the matrix.")

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

(defun-gsl-mdsfc set-all ((object gsl-matrix) value)
  "gsl_matrix_set_all"
  (((pointer object) :pointer) (value :c-base-type))
  :c-return :void)

(defun-gsl-mdsfc set-zero ((object gsl-matrix))
  "gsl_matrix_set_zero"
  (((pointer object) :pointer))
  :c-return :void)

(defun-gsl-mdsfc set-identity ((matrix gsl-matrix))
  "gsl_matrix_set_identity" (((pointer matrix) gsl-matrix-c))
  :c-return :void
  :documentation
  "Set the elements of the matrix @var{m} to the
  corresponding elements of the identity matrix, @math{m(i,j) =
  \delta(i,j)}, i.e. a unit diagonal with all off-diagonal elements zero.
  This applies to both square and rectangular matrices.")

;;;;****************************************************************************
;;;; Matrix Views
;;;;****************************************************************************

(cffi:defcstruct gsl-matrix-view
  (matrix gsl-matrix-c))

(defun-gsl-mdsfc submatrix ((matrix gsl-matrix) k1 k2 n1 n2)
  "gsl_matrix_submatrix"
  (((pointer matrix) gsl-matrix-c) (k1 :size) (k2 :size) (n1 :size) (n2 :size))
  :c-return gsl-matrix-view
  :documentation
  "A matrix view of a submatrix of the matrix
   @var{m}.  The upper-left element of the submatrix is the element
   (@var{k1},@var{k2}) of the original matrix.  The submatrix has @var{n1}
   rows and @var{n2} columns.  The physical number of columns in memory
   given by @var{tda} is unchanged.")

(defun-gsl-mdsfc matrix-array ((matrix gsl-matrix) n1 n2)
  "gsl_matrix_view_array"
  (((pointer matrix) gsl-matrix-c) (n1 :size) (n2 :size))
  :c-return gsl-matrix-view
  :documentation
  "A matrix view of the array @var{base}.  The
  matrix has @var{n1} rows and @var{n2} columns.  The physical number of
  columns in memory is also given by @var{n2}.")

(defun-gsl-mdsfc matrix-array-tda ((matrix gsl-matrix) n1 n2 tda)
  "gsl_matrix_view_array_with_tda"
  (((pointer matrix) gsl-matrix-c) (n1 :size) (n2 :size) (tda :size))
  :c-return gsl-matrix-view
  :documentation
  "A matrix view of the array @var{base} with a
  physical number of columns @var{tda} which may differ from the corresponding
  dimension of the matrix.  The matrix has @var{n1} rows and @var{n2}
  columns, and the physical number of columns in memory is given by
  @var{tda}.")

(defmacro defun-gsl-mvdsfc (&rest args)
  "A defun-gsl for vectors of double, single, fixnum, and complex,
  translating to a GSL function named matrix_*."
  (defun-gsl-all
      '(matrix-double matrix-single matrix-fixnum matrix-complex)
      '(:double :float :int gsl-complex)
    "matrix"
    'gsl-vector
    args))

(defun-gsl-mvdsfc matrix-vector ((v gsl-vector) n1 n2)
  "gsl_matrix_view_vector"
  (((pointer v) gsl-vector-c) (n1 :size) (n2 :size))
  :c-return gsl-matrix-view
  :documentation
  "A matrix view of the vector @var{v}.  The matrix
  has @var{n1} rows and @var{n2} columns. The vector must have unit
  stride. The physical number of columns in memory is also given by
  @var{n2}.  Mathematically, the @math{(i,j)}-th element of the new
  matrix is given by m'(i,j) = v->data[i*n2 + j]
  where the index @var{i} runs from 0 to @code{n1-1} and the index @var{j}
  runs from 0 to @code{n2-1}.
  The new matrix is only a view of the vector @var{v}.  When the view
  goes out of scope the original vector @var{v} will continue to exist.
  The original memory can only be deallocated by freeing the original
  vector.  Of course, the original vector should not be deallocated while
  the view is still in use.")

(defun-gsl-mvdsfc matrix-vector-tda ((v gsl-vector) n1 n2 tda)
    "gsl_matrix_view_vector_with_tda"
  (((pointer v) gsl-vector-c) (n1 :size) (n2 :size) (tda :size))
  :c-return gsl-matrix-view
  :documentation
  "A matrix view of the vector @var{v} with a
  physical number of columns @var{tda} which may differ from the
  corresponding matrix dimension.  The vector must have unit stride. The
  matrix has @var{n1} rows and @var{n2} columns, and the physical number
  of columns in memory is given by @var{tda}.  Mathematically, the
  @math{(i,j)}-th element of the new matrix is given by
  m'(i,j) = v->data[i*tda + j]
  where the index @var{i} runs from 0 to @code{n1-1} and the index @var{j}
  runs from 0 to @code{n2-1}.
  The new matrix is only a view of the vector @var{v}.  When the view
  goes out of scope the original vector @var{v} will continue to exist.
  The original memory can only be deallocated by freeing the original
  vector.  Of course, the original vector should not be deallocated while
  the view is still in use.")

;;;;****************************************************************************
;;;; Row and Column Views
;;;;****************************************************************************

(defun-gsl-mdsfc row-view ((matrix gsl-matrix) i) 
  "gsl_matrix_row" (((pointer matrix) gsl-matrix-c) (i :size))
  :c-return gsl-vector-view
  :null-pointer-info (:EFAULT (format nil "index ~d out of range" i))
  :documentation
  "A vector view of the @var{i}-th row of the matrix.")

(defun-gsl-mdsfc column-view ((matrix gsl-matrix) j)
  "gsl_matrix_column" ((matrix gsl-matrix-c) (j :size))
  :c-return gsl-matrix-view
  :null-pointer-info (:EFAULT (format nil "index ~d out of range" j))
  :documentation
  "A vector view of the @var{j}-th column of the matrix.")

(defun-gsl-mdsfc diagonal-view ((matrix gsl-matrix))
  "gsl_matrix_diagonal" ((matrix gsl-matrix-c))
  :c-return gsl-matrix-view
  :documentation
  "A vector view of the diagonal of the matrix.
   The matrix is not required to be square.
   For a rectangular matrix the length of the diagonal is the same as the smaller
   dimension of the matrix.")

(defun-gsl-mdsfc subdiagonal-view ((matrix gsl-matrix) k)
  "gsl_matrix_subdiagonal" ((matrix gsl-matrix-c) (k :size))
  :c-return gsl-matrix-view
  :documentation
  "A vector view of the @var{k}-th subdiagonal of the matrix; it is not
   required to be square.  The diagonal of the matrix corresponds to
   @math{k = 0}.")

(defun-gsl-mdsfc superdiagonal-view ((matrix gsl-matrix) k)
  "gsl_matrix_superdiagonal"
  ((matrix gsl-matrix-c) (k :size))
  :c-return gsl-matrix-view
  :documentation
  "A vector view of the @var{k}-th superdiagonal of the matrix; it is not
   required to be square. The diagonal of the matrix corresponds to @math{k = 0}.")

;;;;****************************************************************************
;;;; Copying
;;;;****************************************************************************

(defun-gsl-mdsfc copy ((destination gsl-matrix) (source gsl-matrix))
  "gsl_matrix_memcpy"
  (((pointer destination) gsl-matrix-c) ((pointer source) gsl-matrix-c))
  :invalidate (destination)
  :documentation
  "Copy the elements of the matrix @var{source} into the
   matrix @var{destination}.  The two matrices must have the same size.")

(defun-gsl-mdsfc swap ((m1 gsl-matrix) (m2 gsl-matrix))
  "gsl_matrix_swap"
  (((pointer m1) gsl-matrix-c) ((pointer m2) gsl-matrix-c))
  :invalidate (m1 m2)
  :documentation
  "Exchange the elements of the matrices @var{m1} and
   @var{m2} by copying.  The two matrices must have the same size.")

;;;;****************************************************************************
;;;; Copying rows and columns
;;;;****************************************************************************

(defun-gsl-mdsfc row ((vector gsl-vector) (matrix gsl-matrix) i)
  "gsl_matrix_get_row"
  (((pointer vector) gsl-vector-c) ((pointer matrix) gsl-matrix-c) (i :size))
  :invalidate (vector)
  :documentation
  "Copy the elements of the @var{i}-th row of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the row.")

(defun-gsl-mdsfc column ((vector gsl-vector) (matrix gsl-matrix) j)
  "gsl_matrix_get_col"
  (((pointer vector) gsl-vector-c) ((pointer matrix) gsl-matrix-c) (j :size))
  :invalidate (vector)
  :documentation
  "Copy the elements of the @var{j}-th column of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the column.")

(defun-gsl-mdsfc set-row ((matrix gsl-matrix) i (vector gsl-vector))
  "gsl_matrix_set_row"
  (((pointer matrix) gsl-matrix-c) (i :size) ((pointer vector) gsl-vector-c))
  :invalidate (matrix)
  :documentation
  "Copy the elements of the vector into the
   @var{i}-th row of the matrix.  The length of the vector must be
   the same as the length of the row.")

(defun-gsl-mdsfc set-column ((matrix gsl-matrix) j (vector gsl-vector))
  "gsl_matrix_set_col"
  (((pointer matrix) gsl-matrix-c) (j :size) ((pointer vector) gsl-vector-c))
  :invalidate (matrix)
  :documentation
  "Copy the elements of the vector into the @var{j}-th column of the matrix.
  The length of the vector must be the same as the length of the column.")

(defun (setf row) (vector matrix i) (set-row matrix i vector))
(defun (setf column) (vector matrix j) (set-column matrix j vector))

;;;;****************************************************************************
;;;; Exchanging rows and columns
;;;;****************************************************************************

(defun-gsl-mdsfc swap-rows ((matrix gsl-matrix) i j)
  "gsl_matrix_swap_rows"
  (((pointer matrix) gsl-matrix-c) (i :size) (j :size))
  :invalidate (matrix)
  :documentation
  "Exchange the @var{i}-th and @var{j}-th rows of the matrix in-place.")

(defun-gsl-mdsfc swap-columns ((matrix gsl-matrix) i j)
  "gsl_matrix_swap_columns"
  (((pointer matrix) gsl-matrix-c) (i :size) (j :size))
  :invalidate (matrix)
  :documentation
  "Exchange the @var{i}-th and @var{j}-th columns of the matrix in-place.")

(defun-gsl-mdsfc swap-rowcol ((matrix gsl-matrix) i j)
  "gsl_matrix_swap_rowcol"
  (((pointer matrix) gsl-matrix-c) (i :size) (j :size))
  :invalidate (matrix)
  :documentation
  "Exchange the @var{i}-th row and @var{j}-th column of the
   matrix in-place.  The matrix must be square for this operation to
   be possible.")

(defun-gsl-mdsfc matrix-transpose-copy
    ((destination gsl-matrix) (source gsl-matrix))
  "gsl_matrix_transpose_memcpy"
  (((pointer destination) gsl-matrix-c) ((pointer source) gsl-matrix-c))
  :invalidate (destination)
  :documentation
  "Make the destination matrix the transpose of the source matrix
   by copying the elements.  The dimensions of the destination
   matrix must match the transposed dimensions of the source.")

(defun-gsl-mdsfc matrix-transpose ((matrix gsl-matrix))
  "gsl_matrix_transpose"
  (((pointer matrix) gsl-matrix-c))
  :invalidate (matrix)
  :documentation
  "Replace the matrix by its transpose by copying the elements
   of the matrix in-place.  The matrix must be square for this
   operation to be possible.")

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(defun-gsl-mdsfc gsl+ ((a gsl-matrix) (b gsl-matrix))
    "gsl_matrix_add"
  (((pointer a) gsl-matrix-c) ((pointer b) gsl-matrix-c))
  :invalidate (a)
  :documentation
  "Add the elements of matrix @var{b} to the elements of matrix @var{a},
   @math{a'_i = a_i + b_i}. The two matrices must have the
   same dimensions.")

(defun-gsl-mdsfc gsl- ((a gsl-matrix) (b gsl-matrix))
  "gsl_matrix_sub" (((pointer a) gsl-matrix-c) ((pointer b) gsl-matrix-c))
  :invalidate (a)
  :documentation
  "Subtract the elements of matrix @var{b} from the elements of matrix
   @var{a}, @math{a'_i = a_i - b_i}. The two matrices must have the
   same dimensions.")

(defun-gsl-mdsfc gsl* ((a gsl-matrix) (b gsl-matrix))
  "gsl_matrix_mul_elements"
  (((pointer a) gsl-matrix-c) ((pointer b) gsl-matrix-c))
  :invalidate (a)
  :documentation
  "Multiply the elements of matrix @var{a} by the elements of
  matrix @var{b}, @math{a'(i,j) = a(i,j) * b(i,j)}. The two matrices must have the
  same dimensions.")

(defun-gsl-mdsfc gsl/ ((a gsl-matrix) (b gsl-matrix))
  "gsl_matrix_div_elements"
  (((pointer a) gsl-matrix-c) ((pointer b) gsl-matrix-c))
  :invalidate (a)
  :documentation
  "Divide the elements of matrix @var{a} by the elements of
   matrix @var{b}, @math{a'(i,j) = a(i,j) / b(i,j)}. The two matrices must have the
   same dimensions.")

(defun-gsl-mdsfc gsl*c ((a gsl-matrix) x)
  "gsl_matrix_scale" (((pointer a) gsl-matrix-c) (x :c-base-type))
  :invalidate (a)
  :documentation
  "Multiply the elements of matrix @var{a} by the constant
  factor @var{x}, @math{a'(i,j) = x a(i,j)}.")

(defun-gsl-mdsfc gsl+c ((a gsl-matrix) x)
  "gsl_matrix_add_constant" (((pointer a) gsl-matrix-c) (x :c-base-type))
  :invalidate (a)
  :documentation
  "Add the constant value @var{x} to the elements of the
  matrix @var{a}, @math{a'(i,j) = a(i,j) + x}.")

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(defun-gsl-mdsf gsl-max ((m gsl-matrix))
  "gsl_matrix_max" (((pointer m) gsl-matrix-c))
  :documentation
  "The maximum value in the matrix @var{m}."
  :c-return :c-base-type)

(defun-gsl-mdsf gsl-min ((m gsl-matrix))
  "gsl_matrix_min" (((pointer m) gsl-matrix-c))
  :documentation
  "The minimum value in the matrix @var{m}."
  :c-return :c-base-type)

(defun-gsl-mdsf gsl-minmax ((m gsl-matrix))
  "gsl_matrix_minmax"
  (((pointer m) gsl-matrix-c) (min :c-base-type) (max :c-base-type))
  :documentation
  "The minimum and maximum values in the matrix @var{m}."
  :c-return :void)

(defun-gsl-mdsf gsl-max-index ((m gsl-matrix))
  "gsl_matrix_max_index"
  (((pointer m) gsl-matrix-c) (imax :size) (jmax :size))
  :documentation
  "The index of the maximum value in the matrix @var{m}.
   When there are several equal maximum elements then the lowest index is
   returned."
  :c-return :void
  :return ((list (size-to-cl imax) (size-to-cl jmax))))

(defun-gsl-mdsf gsl-min-index ((m gsl-matrix))
  "gsl_matrix_min_index"
  (((pointer m) gsl-matrix-c) (imin :size) (jmin :size))
  :documentation
  "The index of the minimum value in the matrix @var{m}.
  When there are several equal minimum elements then the
  lowest index is returned."
  :c-return :void
  :return ((list (size-to-cl imin) (size-to-cl jmin))))

(defun-gsl-mdsf gsl-minmax-index ((m gsl-matrix))
  "gsl_matrix_minmax_index"
  (((pointer m) gsl-matrix-c)
   (imin :size) (jmin :size) (imax :size) (jmax :size))
  :documentation
  "The indices of the minimum and maximum values in the matrix @var{m}.
  When there are several equal minimum elements then the lowest index is
  returned."
  :c-return :void
  :return ((list (size-to-cl imin) (size-to-cl jmin))
	   (list (size-to-cl imax) (size-to-cl jmax))))

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

(defun-gsl-mdsfc gsl-zerop ((m gsl-matrix))
  "gsl_matrix_isnull" ((m gsl-matrix-c))
  :documentation
  "All elements of matrix @var{m} are zero."
  :c-return :boolean)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(defparameter *intmat-1* (make-data 'matrix-fixnum nil 2 2))
(defparameter *intmat-2* (make-data 'matrix-fixnum nil 2 2))
(defparameter *intmatvec* (make-data 'vector-fixnum nil 2))

(lisp-unit:define-test matrix-fixnum
  (lisp-unit:assert-eql			;(setf gsl-aref), gsl-aref
   77
   (progn
     (setf (gsl-aref *intmat-1* 0 1) 77)
     (gsl-aref *intmat-1* 0 1)))
  (lisp-unit:assert-equalp		;(setf data)
   #2A((4 6) (8 2))
   (progn (setf (data *intmat-1*) #2A((4 6) (8 2))) (data *intmat-1*)))
  (lisp-unit:assert-equalp		;set-zero
   #2A((0 0) (0 0))
   (progn (set-zero *intmat-1*) (data *intmat-1*)))
  (lisp-unit:assert-equalp		;set-all
   #2A((44 44)(44 44))
   (progn (set-all *intmat-1* 44) (data *intmat-1*)))
  (lisp-unit:assert-equalp		;set-basis
   #2A((1 0)(0 1))
   (progn (set-identity *intmat-1*) (data *intmat-1*)))
  (lisp-unit:assert-equalp		;row
   #(4 6)
   (progn
     (setf (data *intmat-1*) #2A((4 6) (8 2)))
     (row *intmatvec* *intmat-1* 0)
     (data *intmatvec*)))
  (lisp-unit:assert-equalp		;column
   #(6 2)
   (progn
     (setf (data *intmat-1*) #2A((4 6) (8 2)))
     (column *intmatvec* *intmat-1* 1)
     (data *intmatvec*)))
  (lisp-unit:assert-eql			;gsl-min
   -12
   (progn
     (setf (data *intmat-1*) #2A((-1 -12) (8 3)))
     (gsl-min *intmat-1*)))
  (lisp-unit:assert-eql			;gsl-max
   8
   (progn
     (setf (data *intmat-1*) #2A((-1 -12) (8 3)))
     (gsl-max *intmat-1*)))
  (lisp-unit:assert-equal		;gsl-minmax
   '(-12 8)
   (progn
     (setf (data *intmat-1*) #2A((-1 -12) (8 3)))
     (multiple-value-list (gsl-minmax *intmat-1*))))
  (lisp-unit:assert-equal		;gsl-min-index
   '(0 1)
   (progn
     (setf (data *intmat-1*) #2A((-1 -12) (8 3)))
     (gsl-min-index *intmat-1*)))
  (lisp-unit:assert-equal		;gsl-max-index
   '(1 0)
   (progn
     (setf (data *intmat-1*) #2A((-1 -12) (8 3)))
     (gsl-max-index *intmat-1*)))
  (lisp-unit:assert-equal		;gsl-minmax-index
   '((0 1) (1 0))
   (progn
     (setf (data *intmat-1*) #2A((-1 -12) (8 3)))
     (multiple-value-list (gsl-minmax-index *intmat-1*))))
  (lisp-unit:assert-equalp		;copy
   #2A((1 2)(3 4))
   (progn
     (setf (data *intmat-1*) #2A((1 2)(3 4)))
     (copy *intmat-2* *intmat-1*) (data *intmat-2*)))
  (lisp-unit:assert-equalp		;swap
   #2A((5 6) (7 8))
   (progn
     (setf (data *intmat-1*) #2A((1 2) (3 4))
	   (data *intmat-2*) #2A((5 6) (7 8)))
     (swap *intmat-1* *intmat-2*)
     (data *intmat-1*)))
  (lisp-unit:assert-equalp		;swap-rows
   #2A((3 4) (1 2))
   (progn
     (setf (data *intmat-1*) #2A((1 2) (3 4)))
     (swap-rows *intmat-1* 0 1)
     (data *intmat-1*)))
  (lisp-unit:assert-equalp		;swap-columns
   #2A((2 1) (4 3))
   (progn
     (setf (data *intmat-1*) #2A((1 2) (3 4)))
     (swap-columns *intmat-1* 0 1)
     (data *intmat-1*)))
  (lisp-unit:assert-equalp		;swap-rowcol
   #2A((2 4) (3 1))
   (progn
     (setf (data *intmat-1*) #2A((1 2) (3 4)))
     (swap-rowcol *intmat-1* 0 1)
     (data *intmat-1*))))

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

#|

(with-data (mat matrix (10 3))
  (loop for i from 0 below 10
	do
	(loop for j from 0 below 3
	      do (setf (gsl-aref mat i j) (+ 0.23d0 j (* 100 i)))))
  (loop for i from 0 below 10
	do
	(loop for j from 0 below 3 do (print (gsl-aref mat i j)))))

(with-data (mat matrix (10 3))
  (loop for i from 0 below 10
	do
	(loop for j from 0 below 3
	      do (setf (gsl-aref mat i j) (+ 0.23d0 j (* 100 i)))))
  (data mat))

#2A((0.23d0 1.23d0 2.23d0)
    (100.23d0 101.23d0 102.23d0)
    (200.23d0 201.23d0 202.23d0)
    (300.23d0 301.23d0 302.23d0)
    (400.23d0 401.23d0 402.23d0)
    (500.23d0 501.23d0 502.23d0)
    (600.23d0 601.23d0 602.23d0)
    (700.23d0 701.23d0 702.23d0)
    (800.23d0 801.23d0 802.23d0)
    (900.23d0 901.23d0 902.23d0))

(with-data (mat matrix (2 2))
  (setf (data mat) #2A((1.0d0 2.0d0) (3.0d0 4.0d0)))
  (with-data (ans matrix (2 2))
    (matrix-copy ans mat)
    (data ans)))
;;; #2A((1.0d0 2.0d0) (3.0d0 4.0d0))

(with-data (mat matrix (2 2))
  (setf (data mat) #2A((1.0d0 2.0d0) (3.0d0 4.0d0)))
  (matrix* mat mat)
  (data mat)))
;;; #2A((1.0d0 4.0d0) (9.0d0 16.0d0))

|#
