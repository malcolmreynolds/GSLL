;; Basis splines.
;; Liam Healy 2008-02-18 14:43:20EST basis-splines.lisp
;; Time-stamp: <2008-02-23 19:36:35EST basis-splines.lisp>
;; $Id: $

(in-package :gsl)

;;; Initializing the B-splines solver

(defgo-s (basis-spline order number-of-breakpoints)
    allocate-basis-spline free-basis-spline nil 2)

(defmfun allocate-basis-spline (order number-of-breakpoints)
  "gsl_bspline_alloc"
  ((order size) (number-of-breakpoints size))
  :c-return :pointer
  :export nil
  :index (letm basis-spline)
  :documentation			; FDL
  "Allocate a workspace for computing B-splines. The number of
   breakpoints is given by number-of-breakpoints.  This leads to n =
   nbreak + k - 2 basis functions where k = order. Cubic B-splines are
   specified by k = 4. The size of the workspace is O(5k + nbreak)."  )

(defmfun free-basis-spline (bspline)
  "gsl_bspline_free"
  ((bspline :pointer))
  :c-return :void
  :export nil
  :index (letm basis-spline)
  :documentation			; FDL
  "Free the memory associated with the workspace of the bspline.")

;;; Constructing the knots vector

(defmfun knots (breakpoints workspace)
  "gsl_bspline_knots"
  ((breakpoints :pointer) (workspace :pointer))
  :documentation			; FDL
  "Compute the knots associated with the given breakpoints and store
   them in the workspace.")

(defmfun uniform-knots (a b workspace)
  "gsl_bspline_knots_uniform"
  ((a :double) (b :double) (workspace :pointer))
  :documentation			; FDL
  "Compute knots uniformly on the interval [a, b] and store
   them in the workspace.")

;;; Evaluation of B-splines

(defmfun evaluate-bspline (x B workspace)
  "gsl_bspline_eval"
  ((x :double) ((pointer B) :pointer) (workspace :pointer))
  :documentation			; FDL
  "Evaluate all B-spline basis functions at the position x and store
   them in the GSL vector B, so that the ith element of B is B_i(x).
   B must be of length n = nbreak + k - 2. This value is
   also stored in the workspace. It is far more
   efficient to compute all of the basis functions at once than to
   compute them individually, due to the nature of the defining
   recurrence relation.")

;;; Examples and unit test

#|

|#

(defun bspline-example (&optional (ncoeffs 8))
  (letm ((order 4)			; cubic
	 (ndata 200)
	 (nbreak (+ ncoeffs 2 (- order)))
	 (bw (basis-spline order nbreak))
	 (mw (fit-workspace ndata ncoeffs))
	 (rng (random-number-generator *mt19937* 0))
	 (B (vector-double ncoeffs))
	 (c (vector-double ncoeffs))
	 (cov (matrix-double ncoeffs ncoeffs))
	 (w (vector-double ndata))
	 (x (vector-double ndata))
	 (y (vector-double ndata))
	 (Xmatrix (matrix-double ndata ncoeffs))
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
