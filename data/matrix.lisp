;********************************************************
; file:        matrix.lisp                        
; description: Matrices
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Tue Apr  4 2006 - 23:26
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

(defclass gsl-matrix (gsl-data)
  ()
  (:documentation "GSL matrix."))

;;; Allocation, freeing, reading and writing
(gsl-data-functions "matrix" 2)

(setf *wrap-types* (acons 'gsl-matrix-c (lambda (x) `(pointer ,x)) *wrap-types*))

;;;;****************************************************************************
;;;; Accessing elements
;;;;****************************************************************************

(defun-gsl gsl-matrix-get ((matrix :pointer) (i :size) (j :size))
  "gsl_matrix_get"
  :return (:double)
  :c-return-value :return
  :documEntation "The @math{(i,j)}-th element of a matrix @var{m}.")

(defmethod gsl-aref ((object gsl-matrix) &rest indices)
  (gsl-matrix-get (pointer object) (first indices) (second indices)))

(defun-gsl gsl-matrix-set ((matrix :pointer) (i :size) (j :size) (value :double))
  "gsl_matrix_set"
  :c-return-value :void
  :documentation "Set the value of the @math{(i,j)}-th element of a
    matrix @var{m} to @var{x}.")

(defmethod (setf gsl-aref) (value (object gsl-matrix) &rest indices)
  (gsl-matrix-set (pointer object) (first indices) (second indices) value))

(defun-gsl gsl-matrix-ptr ((matrix :pointer) (i :size) (j :size))
  "gsl_matrix_ptr"
  :return (:pointer)
  :c-return-value :return
  :documentation "A pointer to the @math{(i,j)}-th element of a
  matrix @var{m}.")

;;;;****************************************************************************
;;;; Initializing elements
;;;;****************************************************************************

(defmethod set-all ((object gsl-matrix) value)
  (funcall
   (defun-gsl :lambda ((pointer :pointer) (value :double))
     "gsl_matrix_set_all"
     :c-return-value :void)
   (pointer object)
   value))

(defmethod set-zero ((object gsl-matrix))
  (funcall
   (defun-gsl :lambda ((pointer :pointer))
     "gsl_matrix_set_zero"
     :return ()
     :c-return-value :void)
   (pointer object)))

(defun-gsl set-identity ((marix gsl-matrix-c))
  "gsl_matrix_set_identity"
  :c-return-value :void
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

(add-wrap-type
 gsl-matrix-view
 (lambda (x)
   `(make-instance 'gsl-matrix
     :pointer
     (cffi:foreign-slot-value ,x 'gsl-matrix-view 'matrix))))

(defun-gsl submatrix
    ((matrix gsl-matrix-c) (k1 :size) (k2 :size) (n1 :size) (n2 :size))
  "gsl_matrix_submatrix"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "A matrix view of a submatrix of the matrix
   @var{m}.  The upper-left element of the submatrix is the element
   (@var{k1},@var{k2}) of the original matrix.  The submatrix has @var{n1}
   rows and @var{n2} columns.  The physical number of columns in memory
   given by @var{tda} is unchanged.")

(defun-gsl matrix-array ((matrix gsl-matrix-c) (n1 :size) (n2 :size))
  "gsl_matrix_view_array"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "A matrix view of the array @var{base}.  The
  matrix has @var{n1} rows and @var{n2} columns.  The physical number of
  columns in memory is also given by @var{n2}.")

(defun-gsl matrix-array-tda
    ((matrix gsl-matrix-c) (n1 :size) (n2 :size) (tda :size))
  "gsl_matrix_view_array_with_tda"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "These functions return a matrix view of the array @var{base} with a
physical number of columns @var{tda} which may differ from the corresponding
dimension of the matrix.  The matrix has @var{n1} rows and @var{n2}
columns, and the physical number of columns in memory is given by
@var{tda}.")

(defun-gsl matrix-vector ((vector gsl-vector-c) (n1 :size) (n2 :size))
  "gsl_matrix_view_vector"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "A matrix view of the vector @var{base}.  The
  matrix has @var{n1} rows and @var{n2} columns.  The physical number of
  columns in memory is also given by @var{n2}.")

(defun-gsl matrix-vector-tda
    ((vector gsl-vector-c) (n1 :size) (n2 :size) (tda :size))
  "gsl_matrix_view_vector_with_tda"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "These functions return a matrix view of the vector @var{base} with a
physical number of columns @var{tda} which may differ from the corresponding
dimension of the matrix.  The matrix has @var{n1} rows and @var{n2}
columns, and the physical number of columns in memory is given by
@var{tda}.")

;;;;****************************************************************************
;;;; Row and Column Views
;;;;****************************************************************************

(defun-gsl row-view ((matrix gsl-matrix-c) (i :size))
  "gsl_matrix_row"
  :c-return-value :return
  :return (gsl-matrix-view)
  :check-null-pointers
  ((:creturn :EFAULT (format nil "index ~d out of range" i)))
  :documentation
  "A vector view of the @var{i}-th row of the matrix.")

(defun-gsl column-view ((matrix gsl-matrix-c) (j :size))
  "gsl_matrix_column"
  :c-return-value :return
  :return (gsl-matrix-view)
  :check-null-pointers
  ((:creturn :EFAULT (format nil "index ~d out of range" j)))
  :documentation
  "A vector view of the @var{j}-th column of the matrix.")

(defun-gsl diagonal-view ((matrix gsl-matrix-c))
  "gsl_matrix_diagonal"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "A vector view of the diagonal of the matrix. The matrix is not required to be square.
   For a rectangular matrix the length of the diagonal is the same as the smaller
   dimension of the matrix.")

(defun-gsl subdiagonal-view ((matrix gsl-matrix-c) (k :size))
  "gsl_matrix_subdiagonal"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "A vector view of the @var{k}-th subdiagonal of the matrix; it is not
   required to be square.  The diagonal of the matrix corresponds to @math{k = 0}.")

(defun-gsl superdiagonal-view ((matrix gsl-matrix-c) (k :size))
  "gsl_matrix_superdiagonal"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "A vector view of the @var{k}-th superdiagonal of the matrix; it is not
   required to be square. The diagonal of the matrix corresponds to @math{k = 0}.")

;;;;****************************************************************************
;;;; Copying
;;;;****************************************************************************

(defun-gsl matrix-copy
    ((destination gsl-matrix-c) (source gsl-matrix-c))
  "gsl_matrix_memcpy"
  :documentation
  "Copy the elements of the matrix @var{source} into the
   matrix @var{destination}.  The two matrices must have the same size.")

(defun-gsl matrix-swap
    ((m1 gsl-matrix-c) (m2 gsl-matrix-c))
  "gsl_matrix_swap"
  :documentation
  "Exchange the elements of the matrices @var{m1} and
   @var{m2} by copying.  The two matrices must have the same size.")

;;;;****************************************************************************
;;;; Copying rows and columns
;;;;****************************************************************************

(defun-gsl row ((vector gsl-vector-c) (matrix gsl-matrix-c) (i :size))
  "gsl_matrix_get_row"
  :documentation
  "Copy the elements of the @var{i}-th row of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the row.")

(defun-gsl column ((vector gsl-vector-c) (matrix gsl-matrix-c) (j :size))
  "gsl_matrix_get_col"
  :documentation
  "Copy the elements of the @var{j}-th column of the matrix
   into the vector.  The length of the vector must be the
   same as the length of the column.")

(defun-gsl set-row ((matrix gsl-matrix-c) (i :size) (vector gsl-vector-c))
  "gsl_matrix_set_row"
  :documentation
  "Copy the elements of the vector into the
   @var{i}-th row of the matrix.  The length of the vector must be
   the same as the length of the row.")

(defun-gsl set-column ((matrix gsl-matrix-c) (j :size) (vector gsl-vector-c))
  "gsl_matrix_set_col"
  :documentation
  "Copy the elements of the vector into the @var{j}-th column of the matrix.
  The length of the vector must be the same as the length of the column.")

(defun (setf row) (vector matrix i) (set-row matrix i vector))
(defun (setf column) (vector matrix j) (set-column matrix j vector))

;;;;****************************************************************************
;;;; Exchanging rows and columns
;;;;****************************************************************************

(defun-gsl swap-rows ((matrix gsl-matrix-c) (i :size) (j :size))
  "gsl_matrix_swap_rows"
  :documentation
  "Exchange the @var{i}-th and @var{j}-th rows of the matrix in-place.")

(defun-gsl swap-columns ((matrix gsl-matrix-c) (i :size) (j :size))
  "gsl_matrix_swap_columns"
  :documentation
  "Exchange the @var{i}-th and @var{j}-th columns of the matrix in-place.")

(defun-gsl swap-rowcol ((matrix gsl-matrix-c) (i :size) (j :size))
  "gsl_matrix_swap_rowcol"
  :documentation
  "Exchange the @var{i}-th row and @var{j}-th column of the
   matrix in-place.  The matrix must be square for this operation to
   be possible.")

(defun-gsl matrix-transpose-copy
    ((destination gsl-matrix-c) (source gsl-matrix-c))
  "gsl_matrix_transpose_memcpy"
  :documentation
  "Make the destination matrix the transpose of the source matrix
   by copying the elements.  The dimensions of the destination
   matrix must match the transposed dimensions of the source.")

(defun-gsl matrix-transpose ((matrix gsl-matrix-c))
  "gsl_matrix_transpose"
  :documentation
  "Replace the matrix by its transpose by copying the elements
   of the matrix in-place.  The matrix must be square for this
   operation to be possible.")

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(defun-gsl matrix+ ((a gsl-matrix-c) (b gsl-matrix-c))
  "gsl_matrix_add"
  :documentation
  "Add the elements of matrix @var{b} to the elements of matrix @var{a},
   @math{a'_i = a_i + b_i}. The two matrices must have the
   same dimensions.")

(defun-gsl matrix- ((a gsl-matrix-c) (b gsl-matrix-c))
  "gsl_matrix_sub"
  :documentation
  "Subtract the elements of matrix @var{b} from the elements of matrix
   @var{a}, @math{a'_i = a_i - b_i}. The two matrices must have the
   same dimensions.")

(defun-gsl matrix* ((a gsl-matrix-c) (b gsl-matrix-c))
  "gsl_matrix_mul_elements"
  :documentation
  "Multiply the elements of matrix @var{a} by the elements of
  matrix @var{b}, @math{a'(i,j) = a(i,j) * b(i,j)}. The two matrices must have the
  same dimensions.")

(defun-gsl matrix/ ((a gsl-matrix-c) (b gsl-matrix-c))
  "gsl_matrix_div_elements"
  :documentation
  "Divide the elements of matrix @var{a} by the elements of
   matrix @var{b}, @math{a'(i,j) = a(i,j) / b(i,j)}. The two matrices must have the
   same dimensions.")

(defun-gsl matrix*c
    ((a gsl-matrix-c) (x :double))
  "gsl_matrix_scale"
  :documentation
  "Multiply the elements of matrix @var{a} by the constant
  factor @var{x}, @math{a'(i,j) = x a(i,j)}.")

(defun-gsl matrix+c
    ((a gsl-matrix-c) (x :double))
  "gsl_matrix_add_constant"
  :documentation
  "Add the constant value @var{x} to the elements of the
  matrix @var{a}, @math{a'(i,j) = a(i,j) + x}.")

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(defun-gsl matrix-max ((m gsl-matrix-c)) "gsl_matrix_max"
  :documentation
  "The maximum value in the matrix @var{m}."
  :c-return-value :return
  :return (:double))

(defun-gsl matrix-min ((m gsl-matrix-c)) "gsl_matrix_min"
  :documentation
  "The minimum value in the matrix @var{m}."
  :c-return-value :return
  :return (:double))

(defun-gsl matrix-minmax ((m gsl-matrix-c))
  "gsl_matrix_minmax"
  :documentation
  "The minimum and maximum values in the matrix @var{m}."
  :c-return-value :void
  :return (:double :double))

(defun-gsl matrix-max-index ((m gsl-matrix-c)) "gsl_matrix_max_index"
  :documentation
  "The index of the maximum value in the matrix @var{m}.
   When there are several equal minimum elements then the lowest index is
   returned."
  :c-return-value :return
  :return (:size))

(defun-gsl matrix-min-index ((m gsl-matrix-c)) "gsl_matrix_min_index"
  :documentation
  "The index of the minimum value in the matrix @var{m}.  When there are several
  equal minimum elements then the lowest index is returned."
  :c-return-value :return
  :return (:size))

(defun-gsl matrix-minmax-index ((m gsl-matrix-c))
  "gsl_matrix_minmax_index"
  :documentation
  "The indices of the minimum and maximum values in the matrix @var{m}.
  When there are several equal minimum elements then the lowest index is
  returned."
  :c-return-value :void
  :return (:size :size))

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

(defun-gsl matrix-zerop ((m gsl-matrix-c))
  "gsl_matrix_isnull"
  :documentation
  "All elements of matrix @var{m} are zero."
  :c-return-value :return
  :return (:boolean))

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

|#
