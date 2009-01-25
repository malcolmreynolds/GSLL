;; Basis splines.
;; Liam Healy 2008-02-18 14:43:20EST basis-splines.lisp
;; Time-stamp: <2009-01-25 10:11:48EST basis-splines.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_bspline.h

;;; Should be subclass of interpolation?

(defmobject basis-spline "gsl_bspline"
  ((order sizet) (number-of-breakpoints sizet))
  "basis spline"
  :documentation			; FDL
  "Allocate a workspace for computing B-splines. The number of
   breakpoints is given by number-of-breakpoints.  This leads to n =
   nbreak + k - 2 basis functions where k = order. Cubic B-splines are
   specified by k = 4. The size of the workspace is O(5k + nbreak).")

;;; Constructing the knots vector
(defmfun knots (breakpoints workspace)
  "gsl_bspline_knots"
  (((mpointer breakpoints) :pointer) ((mpointer workspace) :pointer))
  :documentation			; FDL
  "Compute the knots associated with the given breakpoints and store
   them in the workspace.")

(defmfun uniform-knots (a b workspace)
  "gsl_bspline_knots_uniform"
  ((a :double) (b :double) ((mpointer workspace) :pointer))
  :return (workspace)
  :documentation			; FDL
  "Compute knots uniformly on the interval [a, b] and store
   them in the workspace.")

;;; Evaluation of B-splines

(defmfun evaluate-bspline (x B workspace)
  "gsl_bspline_eval"
  ((x :double) ((mpointer B) :pointer) ((mpointer workspace) :pointer))
  :outputs (B)
  :documentation			; FDL
  "Evaluate all B-spline basis functions at the position x and store
   them in the GSL vector B, so that the ith element of B is B_i(x).
   B must be of length n = nbreak + k - 2. This value is
   also stored in the workspace. It is far more
   efficient to compute all of the basis functions at once than to
   compute them individually, due to the nature of the defining
   recurrence relation.")

;;; Query settings of the B-spline
;;; These are not documented but are in
;;; /usr/include/gsl/gsl_bspline.h; are they officially supported?

(defmfun bspline-ncoeffs (B)
  "gsl_bspline_ncoeffs"
  (((mpointer B) :pointer))
  :c-return sizet)

(defmfun bspline-order (B)
  "gsl_bspline_order"
  (((mpointer B) :pointer))
  :c-return sizet)

(defmfun number-of-breakpoints (B)
  "gsl_bspline_nbreak"
  (((mpointer B) :pointer))
  :c-return sizet
  :documentation "The number of breakpoints of the basis spline B.")

(defmfun breakpoint (i B)
  "gsl_bspline_breakpoint"
  ((i sizet) ((mpointer B) :pointer))
  :c-return :double
  :documentation "The ith breakpoint of the basis spline B.")

;;; Examples and unit test

(defun bspline-example (&optional (ncoeffs 8))
  (let* ((order 4)			; cubic
	 (ndata 200)
	 (nbreak (+ ncoeffs 2 (- order)))
	 (bw (make-basis-spline order nbreak))
	 (mw (make-fit-workspace ndata ncoeffs))
	 (rng (make-random-number-generator *mt19937* 0))
	 (B (make-marray 'double-float :dimensions ncoeffs))
	 (c (make-marray 'double-float :dimensions ncoeffs))
	 (cov (make-marray 'double-float :dimensions (list ncoeffs ncoeffs)))
	 (w (make-marray 'double-float :dimensions ndata))
	 (x (make-marray 'double-float :dimensions ndata))
	 (y (make-marray 'double-float :dimensions ndata))
	 (Xmatrix (make-marray 'double-float :dimensions (list ndata ncoeffs)))
	 (sigma 0.1d0))
    ;; The data to be fitted.
    (dotimes (i ndata)
      (let* ((xi (coerce (* i (/ 15 (1- ndata))) 'double-float))
	     (yi (+ (* (cos xi) (exp (* -0.1d0 xi)))
		    (gaussian rng sigma))))
	(setf (maref x i) xi
	      (maref y i) yi
	      (maref w i) (/ (expt sigma 2)))))
    ;; Uniform breakpoints [0, 15]
    (uniform-knots 0.0d0 15.0d0 bw)
    ;; Fit matrix
    (dotimes (i ndata)
      ;; Compute B_j
      (evaluate-bspline (maref x i) B bw)
      ;; Fill in row i of X
      (dotimes (j ncoeffs)
	(setf (maref Xmatrix i j) (maref B j))))
    ;; Do the fit
    (weighted-linear-mfit Xmatrix w y c cov mw)
    ;; Return the smoothed curve
    (loop for xi from 0.0d0 to 15.0d0 by 0.1d0
       with yval and yerr
       do
       (evaluate-bspline xi B bw)
       (multiple-value-setq (yval yerr)
	 (multi-linear-estimate B c cov))
       collect yval into yvals 
       collect yerr into yerrs
       finally (return (values yvals yerrs)))))
