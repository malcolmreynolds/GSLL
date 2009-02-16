;; Generalized eigensystems for nonsymmetric real matrices
;; Liam Healy 2009-02-16 14:27:20EST nonsymmetric-generalized.lisp
;; Time-stamp: <2009-02-16 15:25:43EST nonsymmetric-generalized.lisp>
;; $Id: $

(in-package :gsl)

(defmobject eigen-gen
    "gsl_eigen_gen" ((n sizet))
    "generalized nonsymmetric eigenvalue workspace"
    :gsl-version (1 10)
    :documentation			; FDL
    "Make a workspace for computing eigenvalues of n-by-n real
    generalized nonsymmetric eigensystems. The size of the workspace
    is O(n).")

(defmobject eigen-genv
    "gsl_eigen_genv" ((n sizet))
    "generalized nonsymmetric eigenvector and eigenvalue workspace"
    :gsl-version (1 10)
    :documentation			; FDL
    "Make a workspace for computin geigenvalues and eigenvectors of
    n-by-n real generalized nonsymmetric eigensystems. The size of the
    workspace is O(7n).")

(defmfun set-parameters-gen
    (ws &optional compute-shur-form-s compute-shur-form-t balance)
  "gsl_eigen_gen_params"
  (((if compute-shur-form-s 1 0) :int)
   ((if compute-shur-form-t 1 0) :int)
   ((if balance 1 0) :int)		; currently ignored
   ((mpointer ws) :pointer))
  :gsl-version (1 10)
  :c-return :void
  :export nil
  :index eigenvalues-gen)

(defmfun eigenvalues-gen
    (A B
       &optional
       (alpha
	(make-marray '(complex double-float) :dimensions (dim0 A)))
       (beta
	(make-marray 'double-float :dimensions (dim0 A)))
       (ws (make-eigen-gen (dim0 A)))
       compute-shur-form-s compute-shur-form-t shur-vectors
       &aux
       (balance nil)			; unused by GSL
       (Q
	(if (eql shur-vectors t)
	    (make-marray 'double-float :dimensions (dimensions A))
	    shur-vectors))
       (Z
	(if (eql shur-vectors t)
	    (make-marray 'double-float :dimensions (dimensions A))
	    shur-vectors)))
  ("gsl_eigen_gen" "gsl_eigen_gen_QZ")
  ((((mpointer A) :pointer) ((mpointer B) :pointer)
    ((mpointer alpha) :pointer) ((mpointer beta) :pointer) ((mpointer ws) :pointer))
   (((mpointer A) :pointer) ((mpointer B) :pointer)
    ((mpointer alpha) :pointer) ((mpointer beta) :pointer)
    ((mpointer Q) :pointer) ((mpointer Z) :pointer)
    ((mpointer ws) :pointer)))
  :before
  ((set-parameters-gen ws compute-shur-form-s compute-shur-form-t balance))
  :gsl-version (1 10)
  :switch (shur-vectors)
  :inputs (A B)
  :outputs (A B alpha beta)
  :return (alpha beta)
  :documentation			; FDL
  "Compute the eigenvalues of the real generalized nonsymmetric matrix
   pair (A, B), and store them as pairs in (alpha, beta), where alpha
   is complex and beta is real. If \beta_i is non-zero, then \lambda =
   \alpha_i / \beta_i is an eigenvalue. Likewise, if \alpha_i is
   non-zero, then \mu = \beta_i / \alpha_i is an eigenvalue of the
   alternate problem \mu A y = B y. The elements of beta are
   normalized to be non-negative.

   If S is desired, it is stored in A on output. If T is desired, it
   is stored in B on output. The ordering of eigenvalues in (alpha,
   beta) follows the ordering of the diagonal blocks in the Schur
   forms S and T. In rare cases, this function may fail to find all
   eigenvalues. If this occurs, an error code is returned.

   If compute-shur-form-s is true, the full Schur form S will be
   computed. If it is NIL, S will not be computed (this is the default
   setting). S is a quasi upper triangular matrix with 1-by-1 and
   2-by-2 blocks on its diagonal. 1-by-1 blocks correspond to real
   eigenvalues, and 2-by-2 blocks correspond to complex eigenvalues.

   If compute-shur-form-t true, the full Schur form T will be
   computed.  If it is NIL, T will not be
   computed (this is the default setting). T is an upper triangular
   matrix with non-negative elements on its diagonal. Any 2-by-2
   blocks in S will correspond to a 2-by-2 diagonal block in T.")

(defmfun eigenvalues-eigenvectors-gen
    (A B
       &optional
       (alpha
	(make-marray '(complex double-float) :dimensions (dim0 A)))
       (beta
	(make-marray 'double-float :dimensions (dim0 A)))
       (eigenvectors
	(make-marray 'double-float :dimensions (dimensions A)))
       (ws (make-eigen-genv (dim0 A)))
       compute-shur-form-s compute-shur-form-t shur-vectors
       &aux
       (balance nil)			; unused by GSL
       (Q
	(if (eql shur-vectors t)
	    (make-marray 'double-float :dimensions (dimensions A))
	    shur-vectors))
       (Z
	(if (eql shur-vectors t)
	    (make-marray 'double-float :dimensions (dimensions A))
	    shur-vectors)))
  ("gsl_eigen_genv" "gsl_eigen_genv_QZ")
  ((((mpointer A) :pointer) ((mpointer B) :pointer)
    ((mpointer alpha) :pointer) ((mpointer beta) :pointer)
    ((mpointer eigenvectors) :pointer)
    ((mpointer ws) :pointer))
   (((mpointer A) :pointer) ((mpointer B) :pointer)
    ((mpointer alpha) :pointer) ((mpointer beta) :pointer)
    ((mpointer eigenvectors) :pointer)
    ((mpointer Q) :pointer) ((mpointer Z) :pointer)
    ((mpointer ws) :pointer)))
  :before
  ((set-parameters-gen ws compute-shur-form-s compute-shur-form-t balance))
  :gsl-version (1 10)
  :switch (shur-vectors)
  :inputs (A B)
  :outputs (A B alpha beta eigenvectors)
  :return (alpha beta eigenvectors)
  :documentation			; FDL
  "Compute eigenvalues and right eigenvectors of the n-by-n real
  generalized nonsymmetric matrix pair (A, B). The eigenvalues are
  stored in (alpha, beta) and the eigenvectors are stored in evec. It
  first calls eigenvalues-gen to compute the eigenvalues, Schur forms,
  and Schur vectors. Then it finds eigenvectors of the Schur forms and
  backtransforms them using the Schur vectors. The Schur vectors are
  destroyed in the process, but can be saved by using setting
  shur-vectors true. The computed eigenvectors are normalized to have
  unit magnitude. On output, (A, B) contains the generalized Schur
  form (S, T). 

   If compute-shur-form-s is true, the full Schur form S will be
   computed. If it is NIL, S will not be computed (this is the default
   setting). S is a quasi upper triangular matrix with 1-by-1 and
   2-by-2 blocks on its diagonal. 1-by-1 blocks correspond to real
   eigenvalues, and 2-by-2 blocks correspond to complex eigenvalues.

   If compute-shur-form-t true, the full Schur form T will be
   computed.  If it is NIL, T will not be
   computed (this is the default setting). T is an upper triangular
   matrix with non-negative elements on its diagonal. Any 2-by-2
   blocks in S will correspond to a 2-by-2 diagonal block in T.")
