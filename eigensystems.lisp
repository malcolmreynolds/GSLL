;; Eigenvectors and eigenvalues
;; Liam Healy, Sun May 21 2006 - 19:52
;; Time-stamp: <2009-01-25 10:37:09EST eigensystems.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_eigen.h

;;; Should symmetric matrices form a subclass of matrices, so that
;;; both eigenvalues and eigenvalues-nonsymm could be methods of the
;;; same function?

;;;;****************************************************************************
;;;; Workspace 
;;;;****************************************************************************

(defmobject eigen-symm
    "gsl_eigen_symm" ((n sizet))
    "symmetric eigenvalue workspace"
    :documentation			; FDL
    "Make a workspace for computing eigenvalues of
  n-by-n real symmetric matrices.  The size of the workspace
  is O(2n).")

(defmobject eigen-nonsymm
    "gsl_eigen_nonsymm" ((n sizet))
    "non-symmetric eigenvalue workspace"
    :gsl-version (1 9)
    :documentation			; FDL
    "Make a workspace for computing eigenvalues of
  n-by-n real non-symmetric matrices.  The size of the workspace
  is O(2n).")

(cffi:defcstruct gsl-nonsymm-ws
  (size sizet)		     ; size of matrices
  (diag :pointer)	     ; diagonal matrix elements from balancing
  (tau :pointer)	     ; Householder coefficients
  (Z :pointer)		     ; pointer to Z matrix
  (balancep :int)	     ; perform balancing transformation?
  (n-evals sizet)	     ; number of eigenvalues found
  (francis-ws :pointer))

(defmobject eigen-symmv
    "gsl_eigen_symmv" ((n sizet))
    "symmetric eigensystem workspace"
    :documentation			; FDL
    "Make a workspace for computing eigenvalues and
  eigenvectors of n-by-n real symmetric matrices.  The size of
  the workspace is O(4n).")

(defmobject eigen-nonsymmv
    "gsl_eigen_nonsymmv" ((n sizet))
    "non-symmetric eigenvalue workspace"
    :gsl-version (1 9)
    :documentation			; FDL
    "Make a workspace for computing for computing eigenvalues and
    eigenvectors of n-by-n real nonsymmetric matrices. The size of the
    workspace is O(5n).")

(defmobject eigen-herm
    "gsl_eigen_herm" ((n sizet))
    "Hermitian eigenvalue workspace"	; FDL
    :documentation			; FDL
    "Make a workspace for computing eigenvalues of
  n-by-n complex Hermitian matrices.  The size of the workspace
  is O(3n).")

(defmobject eigen-hermv
    "gsl_eigen_hermv" ((n sizet))
    "Hermitian eigensystem workspace"
    :documentation			; FDL
    "Make a workspace for computing eigenvalues and
  eigenvectors of n-by-n complex hermitian matrices.  The size of
  the workspace is O(5n).")

;;;;****************************************************************************
;;;; Eigenvalues and eigenvectors
;;;;****************************************************************************

(defmfun eigenvalues
    ((A matrix)
     &optional
     (eigenvalues (make-marray element-type :dimensions (dim0 A)))
     (ws (eltcase complex (make-eigen-herm (dim0 A))
		  t (make-eigen-symm (dim0 A)))))
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

(defmfun set-parameters-nonsymmetric
    (ws &optional compute-shur-form balance)
  "gsl_eigen_nonsymm_params"
  (((if compute-shur-form 1 0) :int)
   ((if balance 1 0) :int) ((mpointer ws) :pointer))
  :gsl-version (1 9)
  :c-return :void
  :export nil
  :index eigenvalues-nonsymm)

(defmfun eigenvalues-nonsymm
    (A
     &optional
     (eigenvalues
      (make-marray '(complex double-float) :dimensions (dim0 A)))
     (ws (make-eigen-nonsymm (dim0 A)))
     compute-shur-form balance shur-vectors
     &aux
     (sv
      (if (eql shur-vectors t)
	  (make-marray 'double-float :dimensions (dimensions A))
	  shur-vectors)))
  ("gsl_eigen_nonsymm" "gsl_eigen_nonsymm_Z")
  ((((mpointer A) :pointer)
    ((mpointer eigenvalues) :pointer) ((mpointer ws) :pointer))
   (((mpointer A) :pointer)
    ((mpointer eigenvalues) :pointer) ((mpointer ws) :pointer)
    ((mpointer sv) :pointer)))
  :before
  ((set-parameters-nonsymmetric ws compute-shur-form balance))
  :gsl-version (1 9)
  :switch (shur-vectors)
  :inputs (A)
  :outputs (A eigenvalues)
  :return
  (eigenvalues
   (cffi:foreign-slot-value (mpointer ws) 'gsl-nonsymm-ws 'n-evals))
  :documentation			; FDL
  "Compute the eigenvalues of the real nonsymmetric matrix A and
  stores them in the vector 'eigenvalues. If T is desired, it is
  stored in the upper portion of A on output. Otherwise, on output,
  the diagonal of A will contain the 1-by-1 real eigenvalues and
  2-by-2 complex conjugate eigenvalue systems, and the rest of A is
  destroyed. In rare cases, this function may fail to find all
  eigenvalues. If this happens, a warning is signalled and the number
  of converged eigenvalues is returned as a second value. The
  converged eigenvalues are stored in the beginning of eval.

  If compute-shur-form is true, the full Schur form T will be computed.
  If it is set to nil, T will not be computed (this is
  the default setting). Computing the full Schur form requires
  approximately 1.5-2 times the number of flops.

  If balance is true, a balancing transformation is applied to the
  matrix prior to computing eigenvalues. This transformation is
  designed to make the rows and columns of the matrix have comparable
  norms, and can result in more accurate eigenvalues for matrices
  whose entries vary widely in magnitude. See Balancing for more
  information. Note that the balancing transformation does not
  preserve the orthogonality of the Schur vectors, so if you wish to
  compute the Schur vectors with you will obtain the Schur vectors of
  the balanced matrix instead of the original matrix. The relationship
  will be

          T = Q^t D^(-1) A D Q

  where Q is the matrix of Schur vectors for the balanced matrix, and D
  is the balancing transformation. Then this function will compute
  a matrix Z which satisfies

          T = Z^(-1) A Z

  with Z = D Q. Note that Z will not be orthogonal. For this reason,
  balancing is not performed by default.")

(defmfun eigenvalues-eigenvectors
    ((A matrix)
     &optional
     (eigenvalues (make-marray element-type :dimensions (dim0 A)))
     (eigenvectors (make-marray element-type :dimensions (dimensions A)))
     (ws (eltcase complex (make-eigen-hermv (dim0 A))
		  t (make-eigen-symmv (dim0 A)))))
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

(defmfun eigenvalues-eigenvectors-nonsymm
    (A
     &optional
     (eigenvalues
      (make-marray '(complex double-float) :dimensions (dim0 A)))
     (eigenvectors
      (make-marray  '(complex double-float) :dimensions (dimensions A)))
     (ws (make-eigen-nonsymmv (dim0 A)))
     shur-vectors
     &aux
     (sv
      (if (eql shur-vectors t)
	  (make-marray 'double-float :dimensions (dimensions A))
	  shur-vectors)))
  ("gsl_eigen_nonsymmv" "gsl_eigen_nonsymmv_Z")
  ((((mpointer A) :pointer)
    ((mpointer eigenvalues) :pointer) ((mpointer eigenvectors) :pointer)
    ((mpointer ws) :pointer))
   (((mpointer A) :pointer)
    ((mpointer eigenvalues) :pointer) ((mpointer eigenvectors) :pointer)
    ((mpointer ws) :pointer) ((mpointer sv) :pointer)))
  :gsl-version (1 9)
  :switch (shur-vectors)
  :inputs (A)
  :outputs (A eigenvalues eigenvectors)
  :return (eigenvalues eigenvectors)
  :documentation			; FDL
  "Compute eigenvalues and right eigenvectors of the n-by-n real
  nonsymmetric matrix A. It first calls #'eigenvalues-nonsymm to
  compute the eigenvalues, Schur form T, and Schur vectors. Then it
  finds eigenvectors of T and backtransforms them using the Schur
  vectors. The Schur vectors are destroyed in the process, but can be
  saved by specifying binding shur-vectors to a vector of length n, or
  t to have it automatically made.  The computed eigenvectors are
  normalized to have unit magnitude. On output, the upper portion of A
  contains the Schur form T.  If #'eigenvalues-nonsymm fails, no
  eigenvectors are computed, and an error code is returned.")

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
 
#|
;; When GSL 1.12 is in place and elt+ is supported for complex, this
;; can be expanded to :double-types, if we can create a hermitian
;; matrix.

(generate-all-array-tests eigensystems5 (double-float)
 (let ((m1 (array-default '(5 5))))
   (multiple-value-bind (eval evec)
       (eigenvalues-eigenvectors (elt+ m1 (matrix-transpose-copy m1)))
     (list (cl-array eval) (cl-array evec)))))
|#

(defun eigenvalue-eigenvectors-example ()
  (let ((evecs (make-marray 'double-float :dimensions '(3 3)))
	(evals (make-marray 'double-float :dimensions 3))
	(mat #m((20.0d0 -10.0d0 0.0d0)
		(-10.0d0 30.0d0 0.0d0)
		(0.0d0 0.0d0 40.0d0))))
    (eigenvalues-eigenvectors mat evals evecs)
    (values (cl-array evals) (cl-array evecs))))

(save-test eigensystems
	   (eigenvalue-eigenvectors-example))

