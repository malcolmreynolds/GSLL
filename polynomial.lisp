;; Polynomials
;; Liam Healy, Tue Mar 21 2006 - 18:33
;; Time-stamp: <2008-07-06 16:57:52EDT polynomial.lisp>
;; $Id$

(in-package :gsl)

;;; Provide autotranslation from CL pure arrays?
;;; Divided differences not complete/tested.

;;;;****************************************************************************
;;;; Polynomial Evaluation
;;;;****************************************************************************

(defmfun polynomial-eval (coefficients x)
  "gsl_poly_eval"
  (((c-pointer coefficients) :pointer) ((dim0 coefficients) sizet) (x :double))
  :documentation			; FDL
  "Evaluate the polyonomial with coefficients at the point x."
  :c-return :double)

;;;;****************************************************************************
;;;; Divided Difference Representation of Polynomials
;;;;****************************************************************************

(defmfun divided-difference (dd xa ya)
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
   divided-differences of (@var{xa},@var{ya}) are stored in the array
   dd, of the same length.")

(defmfun polynomial-eval-divided-difference (dd xa x)
  "gsl_poly_dd_eval"
  (((c-pointer dd) :pointer)
   ((c-pointer xa) :pointer)
   ((dim0 xa) sizet)
   (x :double))
  :inputs (dd xa)
  :c-return :double
  :documentation			; FDL
  "Evaluate the polynomial stored in divided-difference form
   in the arrays dd and xa at the point x.")

(defmfun taylor-divided-difference (coefs xp dd xa workspace)
  "gsl_poly_dd_taylor"
  (((c-pointer coefs) :pointer)
   (xp :double)
   ((c-pointer dd) :pointer)
   ((c-pointer xa) :pointer)
   ((dim0 xa) sizet)
   ((c-pointer workspace) :pointer))
  :inputs (coefs xa)
  :outputs (coefs)
  :documentation			; FDL
  "Convert the divided-difference representation of a
  polynomial to a Taylor expansion.  The divided-difference representation
  is supplied in the arrays dd and xa of the same length.
  On output the Taylor coefficients of the polynomial expanded about the
  point xp are stored in the array coefs which has the same length
  as xa and dd.  A workspace of that length must be provided.")

;;;;****************************************************************************
;;;; Quadratic Equations
;;;;****************************************************************************

(defmfun solve-quadratic (a b c)
  "gsl_poly_solve_quadratic"
  ((a :double) (b :double) (c :double) (root1 :double) (root2 :double))
  :c-return :number-of-answers
  :documentation			; FDL
  "The real roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if the roots are not real, these
   values are NIL.")

(defmfun solve-quadratic-complex (a b c)
  "gsl_poly_complex_solve_quadratic"
  ((a :double) (b :double) (c :double)
   (root1 complex-double-c) (root2 complex-double-c))
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
   (root1 :double) (root2 :double) (root3 :double))
  :c-return :number-of-answers
  :documentation			; FDL
  "Find the real roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  The roots are given
   in ascending order.  Three values are always returned;
   if a root is not real, the value returned for it will be NIL.")

(defmfun solve-cubic-complex (a b c)
  "gsl_poly_complex_solve_cubic"
  ((a :double) (b :double) (c :double)
   (root1 complex-double-c) (root2 complex-double-c) (root3 complex-double-c))
  :c-return :number-of-answers
  :documentation			; FDL
  "Find the complex roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  Three values are always returned;
   if a root does not exist, the value returned for it will be NIL.")

;;;;****************************************************************************
;;;; General Polynomial Equations
;;;;****************************************************************************

(defgo-s (complex-workspace n)
	 allocate-complex-workspace free-complex-workspace)

(defmfun allocate-complex-workspace (n)
  "gsl_poly_complex_workspace_alloc" ((n sizet))
  :c-return :pointer
  :export nil
  :index (letm complex-workspace))

(defmfun free-complex-workspace (ws)
  "gsl_poly_complex_workspace_free" ((ws :pointer))
  :c-return :void
  :export nil
  :index (letm complex-workspace))

(export 'polynomial-solve)
(defun polynomial-solve (coefficients)
  ;; FDL
  "The roots of the general polynomial 
  P(x) = a_0 + a_1 x + a_2 x^2 + ... + a_{n-1} x^{n-1} using 
  balanced-QR reduction of the companion matrix.  The parameter n
  specifies the length of the coefficient array.  The coefficient of the
  highest order term must be non-zero.  The function requires a workspace
  w of the appropriate size.  The n-1 roots are returned in
  the packed complex array z of length 2(n-1), alternating
  real and imaginary parts."
  (let ((len (length coefficients)))
    (letm ((coef (vector-double-float coefficients))
	   (answer (vector-double-float (* 2 (1- len))))
	   (ws (complex-workspace len)))
      (values-list (polynomial-solve-ws coef ws answer)))))

(defmfun polynomial-solve-ws (coefficients workspace answer-pd)
  "gsl_poly_complex_solve"
  (((c-pointer coefficients) :pointer) ((dim0 coefficients) sizet)
   (workspace :pointer) ((c-pointer answer-pd) :pointer))
  :return
  ((loop for i from 0 below (dim0 answer-pd) by 2
	 collect (complex (maref answer-pd i)
			  (maref answer-pd (1+ i)))))
  :export nil
  :index polynomial-solve
  :documentation			; FDL
  "Arguments are:
   a GSL array of coefficients, a workspace, a gsl-array of doubles.")


;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests polynomial
 (letm ((xa (vector-double-float #(0.0d0 1.0d0 2.0d0 3.0d0)))
	(ya (vector-double-float #(2.5d0 7.2d0 32.7d0 91.0d0)))
	(dd (vector-double-float 4)))
   (divided-difference dd xa ya)
   (list
    (polynomial-eval-divided-difference dd xa 0.0d0)
    (polynomial-eval-divided-difference dd xa 1.0d0)
    (polynomial-eval-divided-difference dd xa 2.0d0)
    (polynomial-eval-divided-difference dd xa 3.0d0)))
 (letm ((vec (vector-double-float #(1.0d0 2.0d0 3.0d0))))
   (polynomial-eval vec -1.0d0))
 (solve-quadratic 1.0d0 0.0d0 1.0d0)
 (solve-quadratic 1.0d0 -2.0d0 1.0d0)
 (solve-quadratic-complex 1.0d0 -2.0d0 1.0d0)
 (solve-cubic -6.0d0 -13.0d0 42.0d0)
 (solve-cubic-complex -1.0d0 1.0d0 -1.0d0)
 ;; Example from GSL manual
 (polynomial-solve #(-1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0)))
|#

(LISP-UNIT:DEFINE-TEST POLYNOMIAL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 2.5d0 7.2d0 32.7d0 91.0d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((XA (VECTOR-DOUBLE-FLOAT #(0.0d0 1.0d0 2.0d0 3.0d0)))
	 (YA (VECTOR-DOUBLE-FLOAT #(2.5d0 7.2d0 32.7d0 91.0d0)))
	 (DD (VECTOR-DOUBLE-FLOAT 4)))
      (DIVIDED-DIFFERENCE DD XA YA)
      (LIST
       (POLYNOMIAL-EVAL-DIVIDED-DIFFERENCE DD XA 0.0d0)
       (POLYNOMIAL-EVAL-DIVIDED-DIFFERENCE DD XA 1.0d0)
       (POLYNOMIAL-EVAL-DIVIDED-DIFFERENCE DD XA 2.0d0)
       (POLYNOMIAL-EVAL-DIVIDED-DIFFERENCE DD XA 3.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.0d0)
   (MULTIPLE-VALUE-LIST
    (LETM ((VEC (VECTOR-DOUBLE-FLOAT #(1.0d0 2.0d0 3.0d0))))
      (POLYNOMIAL-EVAL VEC -1.0d0))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST) (LIST))
   (MULTIPLE-VALUE-LIST
    (SOLVE-QUADRATIC 1.0d0 0.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0d0 1.0d0)
   (MULTIPLE-VALUE-LIST (SOLVE-QUADRATIC 1.0d0 -2.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #C(1.0d0 0.0d0) #C(1.0d0 0.0d0))
   (MULTIPLE-VALUE-LIST
    (SOLVE-QUADRATIC-COMPLEX 1.0d0 -2.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -3.000000000000001d0 1.9999999999999996d0
	 7.000000000000001d0)
   (MULTIPLE-VALUE-LIST
    (SOLVE-CUBIC -6.0d0 -13.0d0 42.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #C(-5.551115123125783d-17 -0.9999999999999999d0)
	 #C(-5.551115123125783d-17 0.9999999999999999d0)
	 #C(1.0d0 0.0d0))
   (MULTIPLE-VALUE-LIST
    (SOLVE-CUBIC-COMPLEX -1.0d0 1.0d0 -1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #C(-0.8090169943749477d0 0.5877852522924734d0)
	 #C(-0.8090169943749477d0 -0.5877852522924734d0)
	 #C(0.3090169943749475d0 0.951056516295153d0)
	 #C(0.3090169943749475d0 -0.951056516295153d0)
	 #C(0.9999999999999999d0 0.0d0))
   (MULTIPLE-VALUE-LIST
    (POLYNOMIAL-SOLVE
     #(-1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0)))))
