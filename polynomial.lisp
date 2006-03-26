;********************************************************
; file:        polynomial.lisp                           
; description: Polynomials                               
; date:        Tue Mar 21 2006 - 18:33                   
; author:      Liam M. Healy                             
; modified:    Sat Mar 25 2006 - 22:11
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; To do:
;;; Awaits incorporation of arrays into CFFI.
;;; Divided difference needs to be checked.
;;; finish

;;;;****************************************************************************
;;;; Polynomial Evaluation
;;;;****************************************************************************

;;; (polynomial-eval #(1.0d0 2.0d0 3.0d0) -1.0d0)
(defun-gsl polynomial-eval 
    ((coefficients (:double *)) (x :double))
  "gsl_poly_eval"
  :documentation
  "Evaluate the polyonomial with coefficients at the point x."
  :return (:double)
  :c-return-value :return)

;;;;****************************************************************************
;;;; Divided Difference Representation of Polynomials
;;;;****************************************************************************

;;; Use with-divided-difference to compute the divided difference, an
;;; opaque object that is then passed to 
;;; get-divided-difference, eval-divided-difference or taylor-divided-difference
;;; in the body.

(export '(with-divided-difference))
(defmacro with-divided-difference ((dd xa ya) &body body)
  "Compute the divided difference and bind dd to it.
   This variable may then be passed to divided-difference
   functions in the body."
  `(let ((,dd (find-divided-difference ,xa ,ya)))
    (unwind-protect 
	 (progn ,@body)
      (cffi::foreign-array-free (first ,dd))
      (cffi::foreign-array-free (second ,dd)))))

;;; Do not call this function directly; use with-divided-difference.
(defun find-divided-difference (xa ya)
  "Find the divided difference representation of the interpolating polynomial
   for the points stored in the arrays @var{xa} and @var{ya}."
  (let* ((size (length xa))
	 (dd (foreign-alloc :double :count size))
	 (xac (foreign-alloc :double :count size)))
    (cffi::with-foreign-array (yac ya :double (list size))
      ;; return code not checked
      (cffi:foreign-funcall
       "gsl_poly_dd_init"
       :pointer dd
       :pointer xac
       :pointer yac
       :size size
       :int))
    (list dd xac size)))

(defunx get-divided-difference (dd)
  "Convert the divided difference into an array."
  (cffi::foreign-array-to-lisp
   (first dd) :double (list (third dd))))

(defunx-map eval-divided-difference "gsl_poly_dd_eval" (dd x)
  "Evaluate the polynomial stored in divided-difference form
   at the point @var{x}. Call only within a
   with-divided-difference form."
  (cffi:foreign-funcall
   "gsl_poly_dd_eval"
   :pointer (first dd)
   :pointer (second dd)
   :size (third dd)
   :double x
   :double))

(defunx-map taylor-divided-difference "gsl_poly_dd_taylor" (dd xp)
  "Convert the divided-difference representation of a polynomial
   to a Taylor expansion about the point xp.  Call only within a
   with-divided-difference form."
  (let ((cc (foreign-alloc :double :count (third dd)))
	(workspace (foreign-alloc :double :count (third dd))))
    (unwind-protect
	 (progn
	   ;; Return value not checked.
	   (cffi:foreign-funcall
	    "gsl_poly_dd_taylor"
	    :pointer cc
	    :double xp
	    :pointer (first dd)
	    :pointer (second dd)
	    :size (third dd)
	    :pointer workspace
	    :int)
	   (cffi::foreign-array-to-lisp
	    cc :double (list (third dd))))
      (cffi::foreign-array-free workspace)
      (cffi::foreign-array-free cc))))

;;;;****************************************************************************
;;;; Quadratic Equations
;;;;****************************************************************************

(defun-gsl solve-quadratic ((a :double) (b :double) (c :double))
  "gsl_poly_solve_quadratic"
  :documentation
  "The real roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if the roots are not real, these
   values are NIL."
  :return (:double :double)
  :c-return-value :number-of-answers)

(defun-gsl solve-quadratic-complex ((a :double) (b :double) (c :double))
  "gsl_poly_complex_solve_quadratic"
  :documentation
  "The complex roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if a root does not exist, the
   value returned will be NIL."
  :return (gsl-complex gsl-complex)
  :c-return-value :number-of-answers) 

;;;;****************************************************************************
;;;; Cubic Equations
;;;;****************************************************************************

;;; (solve-cubic -6.0d0 -13.0d0 42.0d0)
;;; -3.0d0
;;; 1.9999999999999996d0
;;; 7.0d0

;;; (solve-cubic -1.0d0 1.0d0 -1.0d0)
;;; 1.0d0
;;; NIL
;;; NIL

(defun-gsl solve-cubic ((a :double) (b :double) (c :double))
  "gsl_poly_solve_cubic"
  :documentation
  "Find the real roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  The roots are given
   in ascending order.  Three values are always returned;
   if a root is not real, the value returned for it will be NIL."
  :return (:double :double :double)
  :c-return-value :number-of-answers)

(defun-gsl solve-cubic-complex ((a :double) (b :double) (c :double))
  "gsl_poly_complex_solve_cubic"
  :documentation
  "Find the complex roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  Three values are always returned;
   if a root does not exist, the value returned for it will be NIL."
  :return (gsl-complex gsl-complex gsl-complex)
  :c-return-value :number-of-answers)

;;;;****************************************************************************
;;;; General Polynomial Equations
;;;;****************************************************************************

;;; See /usr/include/gsl/gsl_poly.h
(cffi:defcstruct poly-complex-workspace
  (nc :size)
  (matrix :pointer))

(export '(with-poly-complex-workspace))
(defmacro with-poly-complex-workspace ((workspace size) &body body)
  "Macro to create and cleanup workspace for polynomial root solver." 
  `(let ((,workspace
	  (funcall
	   (defun-gsl :lambda ((n :size))
	     "gsl_poly_complex_workspace_alloc"
	     :return
	     (poly-complex-workspace)
	     :c-return-value :return)
	   ,size)))
    (unwind-protect 
	 (progn ,@body)
      (funcall
       (defun-gsl :lambda ((,workspace poly-complex-workspace))
	 "gsl_poly_complex_workspace_free"
	 :c-return-value :void)
       ,workspace))))

(defun-gsl polynomial-solve
    ((coefficients (:double n)) (workspace poly-complex-workspace))
  "gsl_poly_complex_solve"
  :documentation
  "The roots of the general polynomial 
@c{$P(x) = a_0 + a_1 x + a_2 x^2 + ... + a_{n-1} x^{n-1}$} 
@math{P(x) = a_0 + a_1 x + a_2 x^2 + ... + a_@{n-1@} x^@{n-1@}} using 
balanced-QR reduction of the companion matrix.  The parameter @var{n}
specifies the length of the coefficient array.  The coefficient of the
highest order term must be non-zero.  The function requires a workspace
@var{w} of the appropriate size.  The @math{n-1} roots are returned in
the packed complex array @var{z} of length @math{2(n-1)}, alternating
real and imaginary parts."
  :return ((gsl-complex (1- n))))

#|
;;; Example from GSL manual
(with-poly-complex-workspace (ws 6)
  (polynomial-solve #(-1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0) ws))
|#
