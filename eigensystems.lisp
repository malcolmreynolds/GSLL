;; Eigenvectors and eigenvalues
;; Liam Healy, Sun May 21 2006 - 19:52
;; Time-stamp: <2008-01-21 11:30:57EST eigensystems.lisp>
;; $Id: $

(in-package :gsl)

;;; Eventually make a with-* macro for the eigen-herm functions.

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

(defun-gsl eigen-symm-alloc (n)
  "gsl_eigen_symm_alloc" ((n :size))
  :documentation			; FDL
  "Allocate a workspace for computing eigenvalues of
  n-by-n real symmetric matrices.  The size of the workspace
  is O(2n)."
  :c-return :pointer)

(defun-gsl eigen-symm-free (w)
  "gsl_eigen_symm_free" ((w :pointer))
  :c-return :void
  :documentation			; FDL
  "Free the memory associated with the workspace w.")

(defun-gsl eigen-symmv-alloc (n)
  "gsl_eigen_symmv_alloc" ((n :size))
  :documentation			; FDL
  "Allocate a workspace for computing eigenvalues and
  eigenvectors of n-by-n real symmetric matrices.  The size of
  the workspace is O(4n)."
  :c-return :pointer)

(defun-gsl eigen-symmv-free (w)
  "gsl_eigen_symmv_free" ((w :pointer))
  :c-return :void
  :documentation			; FDL
  "Free the memory associated with the workspace w.")

(defun-gsl eigenvalues-symmetric (A eval ws)
  "gsl_eigen_symm" ((A gsl-matrix-c) (eval gsl-vector-c) (ws :pointer))
  :documentation			; FDL
  "Eigenvalues of the real symmetric matrix
  A.  Additional workspace of the appropriate size must be provided
  in w.  The diagonal and lower triangular part of A are
  destroyed during the computation, but the strict upper triangular part
  is not referenced.  The eigenvalues are stored in the vector @var{eval}
  and are unordered."
  :invalidate (A eval)
  :return (eval))

(defun-gsl eigenvalues-eigenvectors-symmetric (A eval evec ws)
  "gsl_eigen_symmv"
  ((A gsl-matrix-c) (eval gsl-vector-c) (evec gsl-matrix-c) (ws :pointer))
  :documentation			; FDL
  "The eigenvalues and eigenvectors of the real
  symmetric matrix A.  Additional workspace of the appropriate size
  must be provided in w.  The diagonal and lower triangular part of
  A are destroyed during the computation, but the strict upper
  triangular part is not referenced.  The eigenvalues are stored in the
  vector eval and are unordered.  The corresponding eigenvectors are
  stored in the columns of the matrix evec.  For example, the
  eigenvector in the first column corresponds to the first eigenvalue.
  The eigenvectors are guaranteed to be mutually orthogonal and normalised
  to unit magnitude."
  :invalidate (A eval evec)
  :return (eval evec))

;;;;****************************************************************************
;;;; Complex Hermitian Matrices
;;;;****************************************************************************

(defun-gsl eigen-herm-alloc (n)
  "gsl_eigen_herm_alloc" ((n :size))
  :documentation			; FDL
  "Allocate a workspace for computing eigenvalues of
  n-by-n complex hermitian matrices.  The size of the workspace
  is O(3n)."
  :c-return :pointer)

(defun-gsl eigen-herm-free (w)
  "gsl_eigen_herm_free" ((w :pointer))
  :c-return :void
  :documentation			; FDL
  "Free the memory associated with the workspace w.")

(defun-gsl eigenvalues-hermitian (A eval w)
  "gsl_eigen_herm"
  (((pointer A) gsl-matrix-c) ((pointer eval) gsl-vector-c) (w :pointer))
  :documentation			; FDL
  "Compute the eigenvalues of the complex hermitian matrix
   A.  Additional workspace of the appropriate size must be provided
   in w.  The diagonal and lower triangular part of A are
   destroyed during the computation, but the strict upper triangular part
   is not referenced.  The imaginary parts of the diagonal are assumed to be
   zero and are not referenced. The eigenvalues are stored in the vector
   @var{eval} and are unordered."
  :invalidate (eval A)
  :return (eval))

(defun-gsl eigen-hermv-alloc (n)
  "gsl_eigen_hermv_alloc" ((n :size))
  :documentation			; FDL
  "Allocate a workspace for computing eigenvalues and
  eigenvectors of n-by-n complex hermitian matrices.  The size of
  the workspace is O(5n)."
  :c-return :pointer)

(defun-gsl eigen-hermv-free (w)
  "gsl_eigen_hermv_free" ((w :pointer))
  :c-return :void
  :documentation			; FDL
  "Free the memory associated with the workspace w.")

(defun-gsl eigenvalues-eigenvectors-hermitian (A eval evec w)
  "gsl_eigen_hermv"
  (((pointer A) gsl-matrix-c) ((pointer eval) gsl-vector-c)
   ((pointer evec) gsl-matrix-c) (w :pointer))
  :documentation			; FDL
  "Compute the eigenvalues and eigenvectors of the complex
  hermitian matrix A.  Additional workspace of the appropriate size
  must be provided in w.  The diagonal and lower triangular part of
  A are destroyed during the computation, but the strict upper
  triangular part is not referenced. The imaginary parts of the diagonal
  are assumed to be zero and are not referenced.  The eigenvalues are
  stored in the vector eval and are unordered.  The corresponding
  complex eigenvectors are stored in the columns of the matrix evec.
  For example, the eigenvector in the first column corresponds to the
  first eigenvalue.  The eigenvectors are guaranteed to be mutually
  orthogonal and normalised to unit magnitude."
  :invalidate (A eval evec)
  :return (eval evec))

;;;;****************************************************************************
;;;; Sorting Eigenvalues and Eigenvectors
;;;;****************************************************************************

(cffi:defcenum eigen-sort-type
  "gsl_eigen_sort_t from /usr/include/gsl/gsl_eigen.h."
  :value-ascending :value-descending :absolute-ascending :absolute-descending)

(export 'sort-eigenvalues-eigenvectors)
(defgeneric sort-eigenvalues-eigenvectors (eval evec sort-type)
  (:documentation			; FDL
   "Simultaneously sort the eigenvalues stored in the vector
  eval and the corresponding real eigenvectors stored in the columns
  of the matrix evec into ascending or descending order according to
  the value of the parameter sort-type: :value-ascending,
  :value-descending, :absolute-ascending, :absolute-descending."))

(defun-gsl sort-eigenvalues-eigenvectors
    ((eval gsl-vector-double) evec sort-type)
  "gsl_eigen_symmv_sort"
  ((eval gsl-vector-c) (evec gsl-matrix-c) (sort-type eigen-sort-type))
  :type :method
  :invalidate (eval evec))

(defun-gsl sort-eigenvalues-eigenvectors
    ((eval gsl-vector-complex) evec sort-type)
  "gsl_eigen_hermv_sort"
  ((eval gsl-vector-c) (evec gsl-matrix-c) (sort-type eigen-sort-type))
  :type :method
  :invalidate (eval evec))

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
