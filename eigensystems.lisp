;; Eigenvectors and eigenvalues
;; Liam Healy, Sun May 21 2006 - 19:52
;; Time-stamp: <2008-03-10 21:20:59EDT eigensystems.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Real Symmetric Matrices
;;;;****************************************************************************

(defgo-s (eigen-symm n) eigen-symm-alloc eigen-symm-free)

(defmfun eigen-symm-alloc (n)
  "gsl_eigen_symm_alloc" ((n size))
  :c-return :pointer
  :export nil
  :index '(letm eigen-symm)
  :documentation			; FDL
  "Allocate a workspace for computing eigenvalues of
  n-by-n real symmetric matrices.  The size of the workspace
  is O(2n).")

(defmfun eigen-symm-free (w)
  "gsl_eigen_symm_free" ((w :pointer))
  :c-return :void
  :export nil
  :index '(letm eigen-symm)
  :documentation			; FDL
  "Free the memory associated with the workspace w.")

(defgo-s (eigen-symmv n) eigen-symmv-alloc eigen-symmv-free)

(defmfun eigen-symmv-alloc (n)
  "gsl_eigen_symmv_alloc" ((n size))
  :c-return :pointer
  :export nil
  :index '(letm eigen-symmv)
  :documentation			; FDL
  "Allocate a workspace for computing eigenvalues and
  eigenvectors of n-by-n real symmetric matrices.  The size of
  the workspace is O(4n).")

(defmfun eigen-symmv-free (w)
  "gsl_eigen_symmv_free" ((w :pointer))
  :index '(letm eigen-symmv)
  :c-return :void
  :documentation			; FDL
  "Free the memory associated with the workspace w.")

(defmfun eigenvalues-symmetric (A eval ws)
  "gsl_eigen_symm" ((A gsl-matrix-c) (eval gsl-vector-c) (ws :pointer))
  :documentation			; FDL
  "Eigenvalues of the real symmetric matrix
  A.  Additional workspace of the appropriate size must be provided
  in w.  The diagonal and lower triangular part of A are
  destroyed during the computation, but the strict upper triangular part
  is not referenced.  The eigenvalues are stored in the vector eval
  and are unordered."
  :invalidate (A eval)
  :return (eval))

(defmfun eigenvalues-eigenvectors-symmetric (A eval evec ws)
  "gsl_eigen_symmv"
  (((pointer A) :pointer) ((pointer eval) :pointer)
   ((pointer evec) :pointer) (ws :pointer))
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

(defgo-s (eigen-herm n) eigen-herm-alloc eigen-herm-free)

(defmfun eigen-herm-alloc (n)
  "gsl_eigen_herm_alloc" ((n size))
  :documentation			; FDL
  "Allocate a workspace for computing eigenvalues of
  n-by-n complex hermitian matrices.  The size of the workspace
  is O(3n)."
  :c-return :pointer)

(defmfun eigen-herm-free (w)
  "gsl_eigen_herm_free" ((w :pointer))
  :c-return :void
  :documentation			; FDL
  "Free the memory associated with the workspace w.")

(defmfun eigenvalues-hermitian (A eval w)
  "gsl_eigen_herm"
  (((pointer A) gsl-matrix-c) ((pointer eval) gsl-vector-c) (w :pointer))
  :documentation			; FDL
  "Compute the eigenvalues of the complex hermitian matrix
   A.  Additional workspace of the appropriate size must be provided
   in w.  The diagonal and lower triangular part of A are
   destroyed during the computation, but the strict upper triangular part
   is not referenced.  The imaginary parts of the diagonal are assumed to be
   zero and are not referenced. The eigenvalues are stored in the vector
   eval and are unordered."
  :invalidate (eval A)
  :return (eval))

(defgo-s (eigen-hermv n) eigen-hermv-alloc eigen-hermv-free)

(defmfun eigen-hermv-alloc (n)
  "gsl_eigen_hermv_alloc" ((n size))
  :documentation			; FDL
  "Allocate a workspace for computing eigenvalues and
  eigenvectors of n-by-n complex hermitian matrices.  The size of
  the workspace is O(5n)."
  :c-return :pointer)

(defmfun eigen-hermv-free (w)
  "gsl_eigen_hermv_free" ((w :pointer))
  :c-return :void
  :documentation			; FDL
  "Free the memory associated with the workspace w.")

(defmfun eigenvalues-eigenvectors-hermitian (A eval evec w)
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

(defmfun sort-eigenvalues-eigenvectors
    ((eval vector-double-float) evec sort-type)
  "gsl_eigen_symmv_sort"
  ((eval gsl-vector-c) (evec gsl-matrix-c) (sort-type eigen-sort-type))
  :type :method
  :invalidate (eval evec))

(defmfun sort-eigenvalues-eigenvectors ((eval vector-complex) evec sort-type)
  "gsl_eigen_hermv_sort"
  ((eval gsl-vector-c) (evec gsl-matrix-c) (sort-type eigen-sort-type))
  :type :method
  :invalidate (eval evec))

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************
 
(defun eigenvalue-eigenvectors-example ()
  (letm ((evecs (matrix-double-float 3 3))
	 (evals (vector-double-float 3))
	 (ws (eigen-symmv 3))
	 (mat (matrix-double-float
	       #2A((20.0d0 -10.0d0 0.0d0)
		   (-10.0d0 30.0d0 0.0d0)
		   (0.0d0 0.0d0 40.0d0)))))
    (eigenvalues-eigenvectors-symmetric mat evals evecs ws)
    (values (data evals) (data evecs))))

#|
(make-tests eigensystems
	    (eigenvalue-eigenvectors-example))
|#

(LISP-UNIT:DEFINE-TEST EIGENSYSTEMS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(13.819660112501051d0 36.180339887498945d0 40.0d0)
    #2A((0.8506508083520399d0 -0.5257311121191337d0 0.0d0)
	(0.5257311121191337d0 0.8506508083520399d0 0.0d0)
	(0.0d0 0.0d0 1.0d0)))
   (MULTIPLE-VALUE-LIST
    (EIGENVALUE-EIGENVECTORS-EXAMPLE))))
