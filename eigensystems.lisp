;********************************************************
; file:        eigensystems.lisp
; description: Eigenvectors and eigenvalues
; date:        Sun May 21 2006 - 19:52                   
; author:      Liam M. Healy                             
; modified:    Sun May 21 2006 - 23:22
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Hermitian matrix functions not defined, await complex matrices.


;;;;****************************************************************************
;;;; Workspace macro
;;;;****************************************************************************

(export 'with-eigensystem-workspace)
(defmacro with-eigensystem-workspace
    ((ws n &optional vectors hermitian) &body body)
  "Create a workspace to use in finding eigenvalues and eigenvectors.
   If eigenvectors are desired, set 'vectors to T."
  `(let ((,ws ,(if vectors
		   (if hermitian
		       `(eigen-hermv-alloc ,n)
		       `(eigen-symmv-alloc ,n))
		   (if hermitian
		       `(eigen-herm-alloc ,n)
		       `(eigen-symm-alloc ,n)))))
     (check-null-pointer ,ws :ENOMEM "workspace allocation failed")
     (unwind-protect
	  (progn ,@body)
       ,(if vectors
	    (if hermitian
		`(eigen-hermv-free ,n)
		`(eigen-symmv-free ,ws))
	    (if hermitian
		`(eigen-herm-free ,n)
		`(eigen-symm-free ,ws))))))


;;;;****************************************************************************
;;;; Real Symmetric Matrices
;;;;****************************************************************************

(defun-gsl eigen-symm-alloc ((n :size))
  "gsl_eigen_symm_alloc"
  :return (:pointer)
  :c-return-value :return
  :documentation "Allocate a workspace for computing eigenvalues of
  @var{n}-by-@var{n} real symmetric matrices.  The size of the workspace
  is @math{O(2n)}.")

(defun-gsl eigen-symm-free ((w :pointer))
  "gsl_eigen_symm_free"
  :c-return-value :void
  :documentation "Free the memory associated with the workspace @var{w}.")

(defun-gsl eigen-symmv-alloc ((n :size))
  "gsl_eigen_symmv_alloc"
  :return (:pointer)
  :c-return-value :return
  :documentation "Allocate a workspace for computing eigenvalues and
  eigenvectors of @var{n}-by-@var{n} real symmetric matrices.  The size of
  the workspace is @math{O(4n)}.")

(defun-gsl eigen-symmv-free ((w :pointer))
  "gsl_eigen_symmv_free"
  :c-return-value :void
  :documentation "Free the memory associated with the workspace @var{w}.")

(defun-gsl eigenvalues-symmetric
    ((A gsl-matrix-c) (eval gsl-vector-c) (ws :pointer))
  "gsl_eigen_symm"
  :documentation "Eigenvalues of the real symmetric matrix
  @var{A}.  Additional workspace of the appropriate size must be provided
  in @var{w}.  The diagonal and lower triangular part of @var{A} are
  destroyed during the computation, but the strict upper triangular part
  is not referenced.  The eigenvalues are stored in the vector @var{eval}
  and are unordered."
  :invalidate (A eval)
  :return-input (eval))

(defun-gsl eigenvalues-eigenvectors-symmetric
    ((A gsl-matrix-c) (eval gsl-vector-c) (evec gsl-matrix-c) (ws :pointer))
  "gsl_eigen_symmv"
  :documentation "The eigenvalues and eigenvectors of the real
  symmetric matrix @var{A}.  Additional workspace of the appropriate size
  must be provided in @var{w}.  The diagonal and lower triangular part of
  @var{A} are destroyed during the computation, but the strict upper
  triangular part is not referenced.  The eigenvalues are stored in the
  vector @var{eval} and are unordered.  The corresponding eigenvectors are
  stored in the columns of the matrix @var{evec}.  For example, the
  eigenvector in the first column corresponds to the first eigenvalue.
  The eigenvectors are guaranteed to be mutually orthogonal and normalised
  to unit magnitude."
  :invalidate (A eval evec)
  :return-input (eval evec))

;;;;****************************************************************************
;;;; Complex Hermitian Matrices
;;;;****************************************************************************

(defun-gsl eigen-herm-alloc ((n :size))
  "gsl_eigen_herm_alloc"
  :return (:pointer)
  :c-return-value :return
  :documentation "Allocate a workspace for computing eigenvalues of
  @var{n}-by-@var{n} complex hermitian matrices.  The size of the workspace
  is @math{O(3n)}.")

(defun-gsl eigen-herm-free ((w :pointer))
  "gsl_eigen_herm_free"
  :c-return-value :void
  :documentation "Free the memory associated with the workspace @var{w}.")

(defun-gsl eigen-hermv-alloc ((n :size))
  "gsl_eigen_hermv_alloc"
  :return (:pointer)
  :c-return-value :return
  :documentation "Allocate a workspace for computing eigenvalues and
  eigenvectors of @var{n}-by-@var{n} complex hermitian matrices.  The size of
  the workspace is @math{O(5n)}.")

(defun-gsl eigen-hermv-free ((w :pointer))
  "gsl_eigen_hermv_free"
  :c-return-value :void
  :documentation "Free the memory associated with the workspace @var{w}.")

;;; The rest awaits complex matrices

;;;;****************************************************************************
;;;; Sorting Eigenvalues and Eigenvectors
;;;;****************************************************************************

(cffi:defcenum eigen-sort-type
  "gsl_eigen_sort_t from /usr/include/gsl/gsl_eigen.h."
  :value-ascending :value-descending :absolute-ascending :absolute-descending)

(defun-gsl sort-eigenvalues-eigenvectors
    ((eval gsl-vector-c) (evec gsl-matrix-c) (sort-type eigen-sort-type))
  "gsl_eigen_symmv_sort"
  :documentation "Simultaneously sort the eigenvalues stored in the vector
  @var{eval} and the corresponding real eigenvectors stored in the columns
  of the matrix @var{evec} into ascending or descending order according to
  the value of the parameter @var{sort_type}, :value-ascending,
  :value-descending, :absolute-ascending, :absolute-descending."
  :invalidate (eval evec)
  :return-input (eval evec))

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************
 
#|

(with-data (mat matrix (3 3))
  (setf (data mat)
	#2A((20.0d0 -10.0d0 0.0d0)
	    (-10.0d0 30.0d0 0.0d0)
	    (0.0d0 0.0d0 40.0d0)))
  (with-data (evecs matrix (3 3))
    (with-data (evals vector 3)
      (with-eigensystem-workspace (ws 3 t)
	(eigenvalues-eigenvectors-symmetric mat evals evecs ws)
	(values (data evals) (data evecs))))))

#(13.819660112501051d0 36.180339887498945d0 40.0d0)
#2A((0.8506508083520399d0 -0.5257311121191336d0 0.0d0)
    (0.5257311121191336d0 0.8506508083520399d0 0.0d0)
    (0.0d0 0.0d0 1.0d0))

|#
