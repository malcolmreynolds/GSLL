;; Eigenvectors and eigenvalues
;; Liam Healy, Sun May 21 2006 - 19:52
;; Time-stamp: <2008-12-26 18:57:10EST eigensystems.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_eigen.h

;;;;****************************************************************************
;;;; Workspace 
;;;;****************************************************************************

(defmobject eigen-symm
    "gsl_eigen_symm" ((n sizet))
    "symmetric eigenvalue workspace"	; FDL
    "Make a workspace for computing eigenvalues of
  n-by-n real symmetric matrices.  The size of the workspace
  is O(2n).")

(defmobject eigen-symmv
    "gsl_eigen_symmv" ((n sizet))
    "symmetric eigensystem workspace"	; FDL
    "Make a workspace for computing eigenvalues and
  eigenvectors of n-by-n real symmetric matrices.  The size of
  the workspace is O(4n).")

(defmobject eigen-herm
    "gsl_eigen_herm" ((n sizet))
    "Hermitian eigenvalue workspace"	; FDL
    "Make a workspace for computing eigenvalues of
  n-by-n complex Hermitian matrices.  The size of the workspace
  is O(3n).")

(defmobject eigen-hermv
    "gsl_eigen_hermv" ((n sizet))
    "Hermitian eigensystem workspace"	; FDL
    "Make a workspace for computing eigenvalues and
  eigenvectors of n-by-n complex hermitian matrices.  The size of
  the workspace is O(5n).")

;;;;****************************************************************************
;;;; Eigenvalues and eigenvectors
;;;;****************************************************************************

(defmfun eigenvalues ((A matrix) eigenvalues ws)
  (double-float "gsl_eigen_symm"
   complex-double-float "gsl_eigen_herm")
  (((mpointer A) :pointer)
   ((mpointer eigenvalues) :pointer) ((mpointer ws) :pointer))
  :definition :generic
  :element-types :doubles
  :inputs (A)
  :outputs (A eigenvalues)
  :return (eigenvalues)
  :documentation			; FDL
  "Eigenvalues of the real symmetric or complex hermitian matrix A.
  Additional workspace of the appropriate size and type must be
  provided in w.  The diagonal and lower triangular part of A are
  destroyed during the computation, but the strict upper triangular
  part is not referenced.  For the complex hermitian case, The
  imaginary parts of the diagonal are assumed to be zero and are not
  referenced.  The eigenvalues are stored in the vector eigenvalues and
  are unordered.")

(defmfun eigenvalues-eigenvectors ((A matrix) eigenvalues eigenvectors ws)
  (double-float "gsl_eigen_symmv"
   complex-double-float "gsl_eigen_hermv")  
  (((mpointer A) :pointer) ((mpointer eigenvalues) :pointer)
   ((mpointer eigenvectors) :pointer) ((mpointer ws) :pointer))
  :definition :generic
  :element-types :doubles
  :inputs (A)
  :outputs (A eigenvalues eigenvectors)
  :return (eigenvalues eigenvectors)
  :documentation			; FDL
  "The eigenvalues and eigenvectors of the real symmetric or complex
  hermitian matrix A.  Additional workspace of the appropriate size
  must be provided in w.  The diagonal and lower triangular part of A
  are destroyed during the computation, but the strict upper
  triangular part is not referenced.  For complex hermitian matrices,
  the imaginary parts of the diagonal are assumed to be zero and are
  not referenced.  The eigenvalues are stored in the vector
  eigenvalues and are unordered.  The corresponding eigenvectors are
  stored in the columns of the matrix eigenvectors.  For example, the
  eigenvector in the first column corresponds to the first eigenvalue.
  The eigenvectors are guaranteed to be mutually orthogonal and
  normalised to unit magnitude.")

;;;;****************************************************************************
;;;; Sorting Eigenvalues and Eigenvectors
;;;;****************************************************************************

(cffi:defcenum eigen-sort-type
  ;; gsl_eigen_sort_t from /usr/include/gsl/gsl_eigen.h.
  :value-ascending :value-descending :absolute-ascending :absolute-descending)

(defmfun sort-eigenvalues-eigenvectors
    ((eigenvalues vector) (eigenvectors matrix) sort-type)
  (double-float "gsl_eigen_symmv_sort"
   complex-double-float "gsl_eigen_hermv_sort")
  (((mpointer eigenvalues) :pointer) ((mpointer eigenvectors) :pointer)
   (sort-type eigen-sort-type))
  :definition :generic
  :element-types :doubles
  :inputs (eigenvalues eigenvectors)
  :outputs (eigenvalues eigenvectors)
  :documentation			; FDL
   "Simultaneously sort the eigenvalues stored in the vector
  eigenvalues and the corresponding real eigenvectors stored in the columns
  of the matrix eigenvectors into ascending or descending order according to
  the value of the parameter sort-type: :value-ascending,
  :value-descending, :absolute-ascending, :absolute-descending.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************
 
(defun eigenvalue-eigenvectors-example ()
  (let ((evecs (make-marray 'double-float :dimensions '(3 3)))
	 (evals (make-marray 'double-float :dimensions 3))
	 (ws (make-eigen-symmv 3))
	 (mat #m((20.0d0 -10.0d0 0.0d0)
		 (-10.0d0 30.0d0 0.0d0)
		 (0.0d0 0.0d0 40.0d0))))
    (eigenvalues-eigenvectors mat evals evecs ws)
    (values (cl-array evals) (cl-array evecs))))

(save-test eigensystems
	   (eigenvalue-eigenvectors-example))

