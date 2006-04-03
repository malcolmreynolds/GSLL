;********************************************************
; file:        matrix.lisp                        
; description: Matrices
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Mon Apr  3 2006 - 00:34
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
(gsl-data-functions "matrix")

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
;;;; Views
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

;;;;;;;;; CONVERSION CURTAIN


(defun-gsl view-array ((base :pointer) (size :size))
  "gsl_matrix_view_array"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "A matrix view of an array.  The start of the new
matrix is given by @var{base} and has @var{n} elements.")

(defun-gsl view-array-stride ((base :pointer) (stride :size) (size :size))
  "gsl_matrix_view_array_with_stride"
  :c-return-value :return
  :return (gsl-matrix-view)
  :documentation
  "A matrix view of an array with stride.  The start of the new
matrix is given by @var{base}.")

;;;;****************************************************************************
;;;; Copying
;;;;****************************************************************************

(defun-gsl matrix-copy
    ((destination gsl-matrix-c) (source gsl-matrix-c) )
  "gsl_matrix_memcpy"
  :documentation
  "Copy the elements of the matrix @var{src} into the
   matrix @var{dest}.  The two matrices must have the same length.")

(defun-gsl matrix-swap
    ((v gsl-matrix-c) (w gsl-matrix-c) )
  "gsl_matrix_swap"
  :documentation
  "Exchanges the elements of the matrices @var{v} and @var{w}
   by copying.  The two matrices must have the same length.")

;;;;****************************************************************************
;;;; Exchanging elements
;;;;****************************************************************************

(defun-gsl matrix-swap-elements ((vec gsl-matrix-c) (i :size) (j :size))
  "gsl_matrix_swap_elements"
  :documentation
  "Exchange the @var{i}-th and @var{j}-th elements of the
   matrix @var{v} in-place.")

(defun-gsl matrix-reverse ((vec gsl-matrix-c))
  "gsl_matrix_reverse"
  :documentation
  "Reverse the order of the elements of the matrix @var{v}.")

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(defun-gsl matrix+
    ((a gsl-matrix-c) (b gsl-matrix-c) )
  "gsl_matrix_add"
  :documentation
  "Add the elements of matrix @var{b} to the elements of
matrix @var{a}, @math{a'_i = a_i + b_i}. The two matrices must have the
same length.")

(defun-gsl matrix-
    ((a gsl-matrix-c) (b gsl-matrix-c) )
  "gsl_matrix_sub"
  :documentation
  "Subtract the elements of matrix @var{b} from the elements of
matrix @var{a}, @math{a'_i = a_i - b_i}. The two matrices must have the
same length.")

(defun-gsl matrix*
    ((a gsl-matrix-c) (b gsl-matrix-c) )
  "gsl_matrix_mul"
  :documentation
  "Multiply the elements of matrix @var{a} by the elements of
matrix @var{b}, @math{a'_i = a_i * b_i}. The two matrices must have the
same length.")

(defun-gsl matrix/
    ((a gsl-matrix-c) (b gsl-matrix-c) )
  "gsl_matrix_div"
  :documentation
  "Divide the elements of matrix @var{a} by the elements of
matrix @var{b}, @math{a'_i = a_i / b_i}. The two matrices must have the
same length.")

(defun-gsl matrix*c
    ((a gsl-matrix-c) (x :double))
  "gsl_matrix_scale"
  :documentation
  "Multiply the elements of matrix @var{a} by the constant
factor @var{x}, @math{a'_i = x a_i}.")

(defun-gsl matrix+c
    ((a gsl-matrix-c) (x :double))
  "gsl_matrix_add_constant"
  :documentation
  "Add the constant value @var{x} to the elements of the
matrix @var{a}, @math{a'_i = a_i + x}.")

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(defun-gsl matrix-max ((v gsl-matrix-c)) "gsl_matrix_max"
  :documentation
  "The maximum value in the matrix @var{v}."
  :c-return-value :return
  :return (:double))

(defun-gsl matrix-min ((v gsl-matrix-c)) "gsl_matrix_min"
  :documentation
  "The minimum value in the matrix @var{v}."
  :c-return-value :return
  :return (:double))

(defun-gsl matrix-minmax ((v gsl-matrix-c))
  "gsl_matrix_minmax"
  :documentation
  "The minimum and maximum values in the matrix @var{v}."
  :c-return-value :void
  :return (:double :double))

(defun-gsl matrix-max-index ((v gsl-matrix-c)) "gsl_matrix_max_index"
  :documentation
  "The index of the maximum value in the matrix @var{v}.
   When there are several equal minimum elements then the lowest index is
   returned."
  :c-return-value :return
  :return (:size))

(defun-gsl matrix-min-index ((v gsl-matrix-c)) "gsl_matrix_min_index"
  :documentation
  "The index of the minimum value in the matrix @var{v}.  When there are several
  equal minimum elements then the lowest index is returned."
  :c-return-value :return
  :return (:size))

(defun-gsl matrix-minmax-index ((v gsl-matrix-c))
  "gsl_matrix_minmax_index"
  :documentation
  "The indices of the minimum and maximum values in the matrix @var{v}.
  When there are several equal minimum elements then the lowest index is
  returned."
  :c-return-value :void
  :return (:size :size))

;;;;****************************************************************************
;;;; Properties
;;;;****************************************************************************

(defun-gsl matrix-zerop ((v gsl-matrix-c))
  "gsl_matrix_isnull"
  :documentation
  "All elements of matrix @var{v} are zero."
  :c-return-value :return
  :return (:boolean))

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

#|
(defun print-matrix (vec)
  (format t "~&~f ~f ~f"
	  (gsl-aref vec 0) (gsl-aref vec 1) (gsl-aref vec 2)))

(defun set-matrix (vec &rest values)
  (setf (gsl-aref vec 0) (nth 0 values)
	(gsl-aref vec 1) (nth 1 values)
	(gsl-aref vec 2) (nth 2 values)))

(with-data (vec matrix 3)
  (setf (gsl-aref vec 0) -3.21d0
	(gsl-aref vec 1) 1.0d0
	(gsl-aref vec 2) 12.8d0)
  (print-matrix vec))

(with-data (vec matrix 3)
  (set-all vec 77.8d0)
  (print-matrix vec)
  (set-zero vec)
  (print-matrix vec))

(with-data (vec matrix 3) (set-basis vec 1)
	   (format t "~&~a ~a ~a"
		   (gsl-aref vec 0) (gsl-aref vec 1) (gsl-aref vec 2)))

;;; broken
(with-data (vec matrix 3)
  (setf (gsl-aref vec 0) -3.21d0
	(gsl-aref vec 1) 1.0d0
	(gsl-aref vec 2) 12.8d0)
  (submatrix vec 1 2))

(DEFUN SUBMATRIX (MATRIX OFFSET SIZE)
  (FOREIGN-FUNCALL "gsl_matrix_submatrix"
			  :pointer
			  (POINTER MATRIX)
			  :SIZE
			  OFFSET
			  :SIZE
			  SIZE
			  GSL-MATRIX-VIEW))

(with-data (vec matrix 3)
  (setf (gsl-aref vec 0) -3.21d0
	(gsl-aref vec 1) 1.0d0
	(gsl-aref vec 2) 12.8d0)
  (FOREIGN-FUNCALL "gsl_matrix_submatrix"
			  :pointer
			  (POINTER VEC)
			  :SIZE
			  1
			  :SIZE
			  2
			  GSL-MATRIX-VIEW))

(with-data (vec1 matrix 3)
  (with-data (vec2 matrix 3)
    (setf (gsl-aref vec1 0) -3.21d0
	  (gsl-aref vec1 1) 1.0d0
	  (gsl-aref vec1 2) 12.8d0)
    (matrix-copy vec2 vec1)
    (print-matrix vec2)))

(with-data (vec1 matrix 3)
  (set-matrix vec1 -3.21d0 1.0d0 12.8d0) (print-matrix vec1)
  (matrix-swap vec1 0 1) (print-matrix vec1)
  (matrix-reverse vec1) (print-matrix vec1)
  (matrix*c vec1 2.0d0) (print-matrix vec1)
  (matrix+c vec1 2.0d0) (print-matrix vec1))

(with-data (vec1 matrix 3)
  (with-data (vec2 matrix 3)
    (set-matrix vec1 -3.21d0 1.0d0 12.8d0)
    (set-matrix vec2 1.4d0 17.0d0 -3.0d0)
    (matrix+ vec1 vec2)
    (print-matrix vec1)
    (matrix* vec1 vec2)
    (print-matrix vec1)))

(with-data (vec1 matrix 3)
  (set-matrix vec1 -3.21d0 1.0d0 12.8d0)
  (print-matrix vec1)
  (print (matrix-min vec1))
  (print (matrix-max vec1))
  (matrix-minmax vec1))

(with-data (vec1 matrix 3)
  (set-matrix vec1 -3.21d0 1.0d0 12.8d0)
  (print-matrix vec1)
  (print (matrix-min-index vec1))
  (print (matrix-max-index vec1))
  (matrix-minmax-index vec1))

|#
