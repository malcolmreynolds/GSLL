;; Polynomials
;; Liam Healy, Tue Mar 21 2006 - 18:33
;; Time-stamp: <2009-04-30 22:57:18EDT polynomial.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_poly.h

;;;;****************************************************************************
;;;; Polynomial Evaluation
;;;;****************************************************************************

(defmfun evaluate
    ((coefficients vector-double-float) (x float) &key divided-difference)
  ("gsl_poly_eval" "gsl_poly_dd_eval")
  ((((c-pointer coefficients) :pointer) ((dim0 coefficients) sizet)
    (x :double))
   (((c-pointer divided-difference) :pointer)
    ((c-pointer coefficients) :pointer)
    ((dim0 coefficients) sizet)
    (x :double)))
  :definition :method
  :inputs (coefficients divided-difference)
  :c-return :double
  :documentation			; FDL
  "Evaluate the polyonomial with coefficients at the point x.")

(defmfun evaluate
    ((coefficients vector-double-float) (x complex)
     &key)
  "gsl_poly_complex_eval"
  (((c-pointer coefficients) :pointer) ((dim0 coefficients) sizet)
   (x complex-double-c))
  :definition :method
  :gsl-version (1 11)
  :inputs (coefficients)
  :c-return complex-double-c
  :documentation			; FDL
  "Evaluate the polyonomial with coefficients at the complex value x.")

(defmfun evaluate
    ((coefficients vector-complex-double-float) (x complex)
     &key)
  "gsl_complex_poly_complex_eval"
  (((c-pointer coefficients) :pointer) ((dim0 coefficients) sizet)
   (x complex-double-c))
  :definition :method
  :gsl-version (1 11)
  :inputs (coefficients)
  :c-return complex-double-c
  :documentation			; FDL
  "Evaluate the polyonomial with coefficients at the complex value x.")

;;;;****************************************************************************
;;;; Divided Difference Representation of Polynomials
;;;;****************************************************************************

(defmfun divided-difference
    (xa ya &optional (dd (make-marray 'double-float :dimensions (dim0 xa))))
  "gsl_poly_dd_init"
  (((c-pointer dd) :pointer)
   ((c-pointer xa) :pointer) ((c-pointer ya) :pointer)
   ((dim0 xa) sizet))
  :inputs (xa ya)
  :outputs (dd)
  :return (dd)
  :documentation			; FDL
  "Compute a divided-difference representation of the
   interpolating polynomial for the points (xa, ya) stored in
   the arrays of equal length.  On output the
   divided-differences of (xa,ya) are stored in the array
   dd, of the same length.")

(defmfun taylor-divided-difference
    (xp dd xa
	&optional
	(coefficients (make-marray 'double-float :dimensions (dim0 xa)))
	(workspace (make-marray 'double-float :dimensions (dim0 xa))))
  "gsl_poly_dd_taylor"
  (((c-pointer coefficients) :pointer)
   (xp :double)
   ((c-pointer dd) :pointer)
   ((c-pointer xa) :pointer)
   ((dim0 xa) sizet)
   ((c-pointer workspace) :pointer))
  :inputs (dd xa)
  :outputs (coefficients)
  :documentation			; FDL
  "Convert the divided-difference representation of a
  polynomial to a Taylor expansion.  The divided-difference representation
  is supplied in the arrays dd and xa of the same length.
  On output the Taylor coefficients of the polynomial expanded about the
  point xp are stored in the array coefficients which has the same length
  as xa and dd.")

;;;;****************************************************************************
;;;; Quadratic Equations
;;;;****************************************************************************

(defmfun solve-quadratic (a b c)
  "gsl_poly_solve_quadratic"
  ((a :double) (b :double) (c :double)
   (root1 (:pointer :double)) (root2 (:pointer :double)))
  :c-return :number-of-answers
  :documentation			; FDL
  "The real roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if the roots are not real, these
   values are NIL.")

(defmfun solve-quadratic-complex (a b c)
  "gsl_poly_complex_solve_quadratic"
  ((a :double) (b :double) (c :double)
   (root1 (:pointer complex-double-c)) (root2 (:pointer complex-double-c)))
  :c-return :number-of-answers
  :documentation			; FDL
  "The complex roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if a root does not exist, the
   value returned will be NIL.") 

;;;;****************************************************************************
;;;; Cubic Equations
;;;;****************************************************************************

(defmfun solve-cubic (a b c)
  "gsl_poly_solve_cubic"
  ((a :double) (b :double) (c :double)
   (root1 (:pointer :double))
   (root2 (:pointer :double)) (root3 (:pointer :double)))
  :c-return :number-of-answers
  :documentation			; FDL
  "Find the real roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  The roots are given
   in ascending order.  Three values are always returned;
   if a root is not real, the value returned for it will be NIL.")

(defmfun solve-cubic-complex (a b c)
  "gsl_poly_complex_solve_cubic"
  ((a :double) (b :double) (c :double)
   (root1 (:pointer complex-double-c)) (root2 (:pointer complex-double-c))
   (root3 (:pointer complex-double-c)))
  :c-return :number-of-answers
  :documentation			; FDL
  "Find the complex roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  Three values are always returned;
   if a root does not exist, the value returned for it will be NIL.")

;;;;****************************************************************************
;;;; General Polynomial Equations
;;;;****************************************************************************

(defmobject polynomial-complex-workspace "gsl_poly_complex_workspace"
  ((n sizet))
  "complex workspace for polynomials")

(defmfun polynomial-solve
    (coefficients
     &optional
     (answer (make-marray '(complex double-float)
			  :dimensions (1- (total-size coefficients))))
     (workspace (make-polynomial-complex-workspace (total-size coefficients))))
  "gsl_poly_complex_solve"
  (((c-pointer coefficients) :pointer) ((dim0 coefficients) sizet)
   ((mpointer workspace) :pointer) ((c-pointer answer) :pointer))
  :inputs (coefficients)
  :outputs (answer)
  :return (answer)
  :documentation			; FDL
  "Arguments are: a vector-double-float of coefficients, a complex
   vector of length one less than coefficients that will hold the
   answer, and a workspace made by make-polynomial-complex-workspace.
   The roots of the general polynomial 
   P(x) = a_0 + a_1 x + a_2 x^2 + ... + a_{n-1} x^{n-1} using 
   balanced-QR reduction of the companion matrix.  The coefficient of the
   highest order term must be non-zero.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test polynomial
 (let* ((xa #m(0.0d0 1.0d0 2.0d0 3.0d0))
	(dd (divided-difference xa #m(2.5d0 7.2d0 32.7d0 91.0d0))))
   (list
    (evaluate xa 0.0d0 :divided-difference dd)
    (evaluate xa 1.0d0 :divided-difference dd)
    (evaluate xa 2.0d0 :divided-difference dd)
    (evaluate xa 3.0d0 :divided-difference dd)))
 (let ((vec #m(1.0d0 2.0d0 3.0d0)))
   (evaluate vec -1.0d0))
 (solve-quadratic 1.0d0 0.0d0 1.0d0)
 (solve-quadratic 1.0d0 -2.0d0 1.0d0)
 (solve-quadratic-complex 1.0d0 -2.0d0 1.0d0)
 (solve-cubic -6.0d0 -13.0d0 42.0d0)
 (solve-cubic-complex -1.0d0 1.0d0 -1.0d0)
 ;; Example from GSL manual
 (copy (polynomial-solve #m(-1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0)) 'array)
 ;; tests from gsl-1.11/poly/test.c
 (evaluate #m(1 0.5 0.3) 0.5d0)
 (evaluate #m(1 -1 1 -1 1 -1 1 -1 1 -1 1) 1.0d0)
 (solve-quadratic 4.0d0 -20.0d0 26.0d0)		  ; no roots
 (solve-quadratic 4.0d0 -20.0d0 25.0d0)		  ; one root
 (solve-quadratic 4.0d0 -20.0d0 21.0d0)		  ; two roots
 (solve-quadratic 4.0d0 7.0d0 0.0d0)		  ; two roots
 (solve-quadratic 5.0d0 0.0d0 -20.0d0)		  ; two roots
 (solve-quadratic 0.0d0 3.0d0 -21.0d0)		  ; one root (linear)
 (solve-quadratic 0.0d0 0.0d0 1.0d0)
 (solve-cubic 0.0d0 0.0d0 -27.0d0)		      ; one root
 (solve-cubic -51.0d0 867.0d0 -4913.0d0)	      ; three roots
 (solve-cubic -57.0d0 1071.0d0 -6647.0d0)	      ; three roots
 (solve-cubic -11.0d0 -493.0d0 +6647.0d0)	      ; three roots
 (solve-cubic -143.0d0 5087.0d0 -50065.0d0)	      ; three roots
 (solve-cubic -109.0d0 803.0d0 50065.0d0)	      ; three roots
 (solve-quadratic-complex 4.0d0 -20.0d0 26.0d0)
 (solve-quadratic-complex 4.0d0 -20.0d0 25.0d0)
 (solve-quadratic-complex 4.0d0 -20.0d0 21.0d0)
 (solve-quadratic-complex 4.0d0 7.0d0 0.0d0)
 (solve-quadratic-complex 5.0d0 0.0d0 -20.0d0)
 (solve-quadratic-complex 5.0d0 0.0d0 20.0d0)
 (solve-quadratic-complex 0.0d0 3.0d0 -21.0d0)
 (solve-quadratic-complex 0.0d0 0.0d0 1.0d0)
 (solve-cubic-complex 0.0d0 0.0d0 -27.0d0)
 (solve-cubic-complex -1.0d0 1.0d0 39.0d0)
 (solve-cubic-complex -51.0d0 867.0d0 -4913.0d0)
 (solve-cubic-complex -57.0d0 1071.0d0 -6647.0d0)
 (solve-cubic-complex -11.0d0 -493.0d0 +6647.0d0)
 (solve-cubic-complex -143.0d0 5087.0d0 -50065.0d0)
 (copy (polynomial-solve #m(-120 274 -225 85 -15 1.0)) 'array)
 (copy (polynomial-solve #m(1 0 0 0 1 0 0 0 1)) 'array)
 (let* ((xa #m(0.16 0.97 1.94 2.74 3.58 3.73 4.70))
	(ya #m(0.73 1.11 1.49 1.84 2.30 2.41 3.07))
	(dd (divided-difference xa ya)))
   (list
    (copy dd 'array)
    (map 'vector (lambda (x) (evaluate xa x :divided-difference dd)) (copy xa 'array))
    (map 'vector
	 (lambda (x) (evaluate (taylor-divided-difference 1.5d0 dd xa) (- x 1.5d0)))
	 (copy xa 'array)))))

