;; Singular Value Decomposition
;; Liam Healy, Tue May  2 2006 - 12:15
;; Time-stamp: <2009-09-18 16:07:28EDT svd.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_linalg.h

;;; FDL
;;; A general rectangular M-by-N matrix A has a
;;; singular value decomposition (svd) into the product of an
;;; M-by-N orthogonal matrix U, an N-by-N diagonal matrix of
;;; singular values S and the transpose of an
;;; N-by-N orthogonal square matrix V, A = U S V^T
;;; The singular values sigma_i = S_{ii} are all non-negative and are
;;; generally chosen to form a non-increasing sequence 
;;; sigma_1 >= sigma_2 >= ... >= sigma_N >= 0.

;;; The singular value decomposition of a matrix has many practical uses.
;;; The condition number of the matrix is given by the ratio of the largest
;;; singular value to the smallest singular value. The presence of a zero
;;; singular value indicates that the matrix is singular. The number of
;;; non-zero singular values indicates the rank of the matrix.  In practice
;;; singular value decomposition of a rank-deficient matrix will not produce
;;; exact zeroes for singular values, due to finite numerical
;;; precision. Small singular values should be edited by choosing a suitable
;;; tolerance.

(defmfun SV-decomposition
    (A &optional
       (S (make-marray 'double-float :dimensions (dim1 A)))
       (V (make-marray 'double-float :dimensions (list (dim1 A) (dim1 A))))
       (work (make-marray 'double-float :dimensions (dim1 A))))
  "gsl_linalg_SV_decomp"
  (((mpointer A) :pointer) ((mpointer V) :pointer)
   ((mpointer S) :pointer) ((mpointer work) :pointer))
  :inputs (A)
  :outputs (A S V)
  :return (A S V)
  :documentation			; FDL
  "Factorize the M-by-N matrix A into
  the singular value decomposition A = U S V^T for M >= N.
  On output the matrix A is replaced by U.  The diagonal elements
  of the singular value matrix S
  are stored in the vector S.  The singular values are non-negative
  and form a non-increasing sequence from S_1 to S_N. The
  matrix V contains the elements of V in untransposed
  form. To form the product U S V^T it is necessary to take the
  transpose of V.  A workspace of length N is required in work.
  This routine uses the Golub-Reinsch SVD algorithm.")

(defmfun SV-modified-decomposition
    (A 
     &optional
     (S (make-marray 'double-float :dimensions (dim1 A)))
     (V (make-marray 'double-float :dimensions (list (dim1 A) (dim1 A))))
     (X (make-marray 'double-float :dimensions (list (dim1 A) (dim1 A))))
     (work (make-marray 'double-float :dimensions (dim1 A))))
  "gsl_linalg_SV_decomp_mod"
  (((mpointer A) :pointer) ((mpointer X) :pointer)
   ((mpointer V) :pointer)
   ((mpointer S) :pointer) ((mpointer work) :pointer))
  :inputs (A)
  :outputs (A S V)
  :return (S V)			    ; is A modified by this algorithm?
  :documentation		    ; FDL
  "The SVD using the modified Golub-Reinsch algorithm, which is
   faster for M >> N.  It requires the vector work of length N and the
   N-by-N matrix X as additional working space.")

(defmfun SV-jacobi-decomposition
    (A &optional
       (S (make-marray 'double-float :dimensions (dim1 A)))
       (V (make-marray 'double-float :dimensions (list (dim1 A) (dim1 A)))))
  "gsl_linalg_SV_decomp_jacobi"
  (((mpointer A) :pointer) ((mpointer V) :pointer)
   ((mpointer S) :pointer))
  :inputs (A)
  :outputs (A S V)
  :return (S V)			    ; is A modified by this algorithm?
  :documentation		    ; FDL
  "The SVD of the M-by-N matrix A using one-sided Jacobi
   orthogonalization for M >= N.  The Jacobi method can compute singular
   values to higher relative accuracy than Golub-Reinsch algorithms (see
   references for details).")

(defmfun SV-solve
    (U S V b &optional (x (make-marray 'double-float :dimensions (dim0 U))))
  "gsl_linalg_SV_solve"
  (((mpointer U) :pointer) ((mpointer V) :pointer)
   ((mpointer S) :pointer)
   ((mpointer b) :pointer) ((mpointer x) :pointer))
  :inputs (U S V b)
  :outputs (x)
  :return (x)
  :documentation			; FDL
  "Solve the system A x = b using the singular value
   decomposition (U, S, V) of A given by #'SV-decomposition.

   Only non-zero singular values are used in computing the solution. The
   parts of the solution corresponding to singular values of zero are
   ignored.  Other singular values can be edited out by setting them to
   zero before calling this function. 

   In the over-determined case where A has more rows than columns the
   system is solved in the least squares sense, returning the solution
   x which minimizes ||A x - b||_2.")

;;; Examples and unit test, from linalg/test.c
;;; These are general to all the linear solver techniques, so
;;; more tests need to be made.

(defun create-hilbert-matrix (dim)
  "Make Hilbert matrix used to test linear algebra functions."
  (let ((matrix (make-marray 'double-float :dimensions (list dim dim))))
    (dotimes (i dim matrix)
      (dotimes (j dim)
	(setf (maref matrix i j) (coerce (/ (+ 1 i j)) 'double-float))))))

(defun create-vandermonde-matrix (dim)
  "Make Van der Monde matrix used to test linear algebra functions."
  (let ((matrix (make-marray 'double-float :dimensions (list dim dim))))
    (dotimes (i dim matrix)
      (dotimes (j dim)
	(setf (maref matrix i j)
	      (coerce (expt (1+ i) (- dim j 1)) 'double-float))))))

(defun test-sv-solve-dim (matrix)
  "Solve the linear equation using SVD with the supplied matrix and
   a right-hand side vector which is the reciprocal of one more than
   the index."
  (let* ((dim (dim0 matrix))
	 (rhs (make-marray 'double-float :dimensions dim)))
    (dotimes (i dim)
      (setf (maref rhs i) (coerce (1+ i) 'double-float)))
    (multiple-value-bind (u q d)
	(SV-decomposition (copy matrix))
      (SV-solve u q d rhs))))

(save-test svd
 (test-sv-solve-dim (create-hilbert-matrix 2))
 (test-sv-solve-dim (create-hilbert-matrix 3))
 (test-sv-solve-dim (create-hilbert-matrix 4))
 (test-sv-solve-dim (create-hilbert-matrix 12))
 (test-sv-solve-dim (create-vandermonde-matrix 2))
 (test-sv-solve-dim (create-vandermonde-matrix 3))
 (test-sv-solve-dim (create-vandermonde-matrix 4))
 (test-sv-solve-dim (create-vandermonde-matrix 12)))
