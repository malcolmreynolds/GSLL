;; Dirichlet distribution
;; Liam Healy, Sun Oct 29 2006
;; Time-stamp: <2008-09-14 22:08:40EDT dirichlet.lisp>
;; $Id$

(in-package :gsl)

(defmfun dirichlet (generator alpha theta)
  "gsl_ran_dirichlet"
  (((generator generator) :pointer)
   ((dim0 alpha) sizet)
   ((c-pointer alpha) :pointer)
   ;; theta had better be at least as long as alpha, or they'll be trouble
   ((c-pointer theta) :pointer))
  :c-return :void
  :documentation			; FDL
  "An array of K=(length alpha) random variates from a Dirichlet
  distribution of order K-1.  The distribution function is
  p(\theta_1,\ldots,\theta_K) \, d\theta_1 \cdots d\theta_K = 
        {1 \over Z} \prod_{i=1}^{K} \theta_i^{\alpha_i - 1} 
          \; \delta(1 -\sum_{i=1}^K \theta_i) d\theta_1 \cdots d\theta_K
  theta_i >= 0 and alpha_i >= 0.
  The delta function ensures that \sum \theta_i = 1.
  The normalization factor Z is
  Z = {\prod_{i=1}^K \Gamma(\alpha_i) \over \Gamma( \sum_{i=1}^K \alpha_i)}
  The random variates are generated by sampling K values 
  from gamma distributions with parameters a=alpha_i, b=1, 
  and renormalizing. 
  See A.M. Law, W.D. Kelton, \"Simulation Modeling and Analysis\"
  (1991).")

(defmfun dirichlet-pdf (alpha theta)
  "gsl_ran_dirichlet_pdf"
  (((1- (dim0 alpha)) sizet)
   ((c-pointer alpha) :pointer)
   ;; theta had better be at least as long as alpha, or they'll be trouble
   ((c-pointer theta) :pointer))
  :c-return :double
  :documentation			; FDL
  "The probability density p(\theta_1, ... , \theta_K)
   at theta[K] for a Dirichlet distribution with parameters 
   alpha[K], using the formula given for #'dirichlet.")

(defmfun dirichlet-log-pdf (alpha theta)
  "gsl_ran_dirichlet_lnpdf"
  (((1- (dim0 alpha)) sizet)
   ((c-pointer alpha) :pointer)
   ;; theta had better be at least as long as alpha, or they'll be trouble
   ((c-pointer theta) :pointer))
  :c-return :double
  :documentation			; FDL
  "The logarithm of the probability density 
   p(\theta_1, ... , \theta_K)
   for a Dirichlet distribution with parameters 
   alpha[K].")

;;; Examples and unit test
#|
(make-tests dirichlet
  (letm ((rng (random-number-generator *mt19937* 0))
	   (alpha (vector-double-float (a 1.0d0 2.0d0 3.0d0 4.0d0)))
	   (theta (vector-double-float 4)))
      (dirichlet rng alpha theta)
      (cl-array theta))
  (letm ((alpha (vector-double-float (a 1.0d0 2.0d0 3.0d0 4.0d0)))
	  (theta (vector-double-float (a 0.1d0 0.3d0 0.4d0 0.2d0))))
     (dirichlet-pdf alpha theta))
  (letm ((alpha (vector-double-float (a 1.0d0 2.0d0 3.0d0 4.0d0)))
	  (theta (vector-double-float (a 0.1d0 0.3d0 0.4d0 0.2d0))))
     (dirichlet-log-pdf alpha theta)))
|#

(LISP-UNIT:DEFINE-TEST DIRICHLET
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(3.9283332456352124d-5 0.468176310887376d0
      0.34075044031099977d0 0.19103396546916782d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0))
	 (ALPHA (VECTOR-DOUBLE-FLOAT (A 1.0d0 2.0d0 3.0d0 4.0d0)))
	 (THETA (VECTOR-DOUBLE-FLOAT 4)))
      (DIRICHLET RNG ALPHA THETA) (CL-ARRAY THETA))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.8800000000000043d0)
   (MULTIPLE-VALUE-LIST
    (LETM
	((ALPHA (VECTOR-DOUBLE-FLOAT (A 1.0d0 2.0d0 3.0d0 4.0d0)))
	 (THETA (VECTOR-DOUBLE-FLOAT (A 0.1d0 0.3d0 0.4d0 0.2d0))))
      (DIRICHLET-PDF ALPHA THETA))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.057790294147856d0)
   (MULTIPLE-VALUE-LIST
    (LETM
	((ALPHA (VECTOR-DOUBLE-FLOAT (A 1.0d0 2.0d0 3.0d0 4.0d0)))
	 (THETA (VECTOR-DOUBLE-FLOAT (A 0.1d0 0.3d0 0.4d0 0.2d0))))
      (DIRICHLET-LOG-PDF ALPHA THETA)))))

