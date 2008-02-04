;; Polynomials
;; Liam Healy, Tue Mar 21 2006 - 18:33
;; Time-stamp: <2008-02-03 15:07:32EST polynomial.lisp>
;; $Id: $

(in-package :gsl)

;;; Provide autotranslation from CL pure arrays?
;;; Divided differences not complete/tested.

;;;;****************************************************************************
;;;; Polynomial Evaluation
;;;;****************************************************************************

(defun-gsl polynomial-eval (coefficients x)
  "gsl_poly_eval"
  (((gsl-array coefficients) :pointer) ((dim0 coefficients) :size) (x :double))
  :documentation			; FDL
  "Evaluate the polyonomial with coefficients at the point x."
  :c-return :double)

;;;;****************************************************************************
;;;; Divided Difference Representation of Polynomials
;;;;****************************************************************************

(defun-gsl divided-difference-int (dd xa ya)
  "gsl_poly_dd_init"
  (((gsl-array dd) :pointer)
   ((gsl-array xa) :pointer) ((gsl-array ya) :pointer)
   ((dim0 xa) :size))
  :return (dd)
  :export nil
  :index divided-difference)

(export '(divided-difference))
(defun divided-difference (xa ya)
  ;; FDL
  "Compute a divided-difference representation of the
  interpolating polynomial for the points (xa, ya) stored in
  the arrays xa and ya.  The output is the
  divided-differences of (xa,ya) stored in an gsl-vector
  of the same length as xa and ya."
  (letm ((xad (vector-double xa))
	 (yad (vector-double ya)))
    (divided-difference-int
     (make-data 'vector-double nil (length xa))
     xad yad)))))

(defun-gsl polynomial-eval-divided-difference (dd xa x)
  "gsl_poly_dd_eval"
  (((gsl-array dd) :pointer)
   ((gsl-array xa) :pointer)
   ((dim0 xa) :size)
   (x :double))
  :c-return :double
  :documentation			; FDL
  "Evaluate the polynomial stored in divided-difference form
   in the arrays dd and xa at the point x.")

(defun-gsl taylor-divided-difference (coefs xp dd xa workspace)
  "gsl_poly_dd_taylor"
  (((gsl-array coefs) :pointer)
   (xp :double)
   ((gsl-array dd) :pointer)
   ((gsl-array xa) :pointer)
   ((dim0 xa) :size)
   ((gsl-array workspace) :pointer))
  :invalidate (coefs)
  :documentation			; FDL
  "Convert the divided-difference representation of a
  polynomial to a Taylor expansion.  The divided-difference representation
  is supplied in the arrays dd and xa of the same length.
  On output the Taylor coefficients of the polynomial expanded about the
  point xp are stored in the array coefs which has the same length
  as xa and dd.  A workspace of length must be provided.")

;;;;****************************************************************************
;;;; Quadratic Equations
;;;;****************************************************************************

(defun-gsl solve-quadratic (a b c)
  "gsl_poly_solve_quadratic"
  ((a :double) (b :double) (c :double) (root1 :double) (root2 :double))
  :c-return :number-of-answers
  :documentation			; FDL
  "The real roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if the roots are not real, these
   values are NIL.")

(defun-gsl solve-quadratic-complex (a b c)
  "gsl_poly_complex_solve_quadratic"
  ((a :double) (b :double) (c :double) (root1 gsl-complex) (root2 gsl-complex))
  :c-return :number-of-answers
  :documentation			; FDL
  "The complex roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if a root does not exist, the
   value returned will be NIL.") 

;;;;****************************************************************************
;;;; Cubic Equations
;;;;****************************************************************************

(defun-gsl solve-cubic (a b c)
  "gsl_poly_solve_cubic"
  ((a :double) (b :double) (c :double)
   (root1 :double) (root2 :double) (root3 :double))
  :c-return :number-of-answers
  :documentation			; FDL
  "Find the real roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  The roots are given
   in ascending order.  Three values are always returned;
   if a root is not real, the value returned for it will be NIL.")

(defun-gsl solve-cubic-complex (a b c)
  "gsl_poly_complex_solve_cubic"
  ((a :double) (b :double) (c :double)
   (root1 gsl-complex) (root2 gsl-complex) (root3 gsl-complex))
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

(defun-gsl allocate-complex-workspace (n)
  "gsl_poly_complex_workspace_alloc" ((n :size))
  :c-return :pointer
  :export nil
  :index (letm complex-workspace))

(defun-gsl free-complex-workspace (ws)
  "gsl_poly_complex_workspace_free" ((ws :pointer))
  :c-return :void
  :export nil
  :index (letm complex-workspace))

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
    (letm ((coef (vector-double coefficients))
	   (answer (vector-double (* 2 (1- len))))
	   (ws (complex-workspace len)))
      (values-list (polynomial-solve-ws coef ws answer)))))

(defun-gsl polynomial-solve-ws (coefficients workspace answer-pd)
  "gsl_poly_complex_solve"
  (((gsl-array coefficients) :pointer) ((dim0 coefficients) :size)
   (workspace :pointer) ((gsl-array answer-pd) :pointer))
  :return
  ((loop for i from 0 below (dim0 answer-pd) by 2
	 collect (complex (gsl-aref answer-pd i)
			  (gsl-aref answer-pd (1+ i)))))
  :documentation			; FDL
  "Arguments are:
   a GSL array of coefficients, a workspace, a gsl-array of doubles."
  :export nil
  :index polynomial-solve)


;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test polynomial
  (lisp-unit:assert-first-fp-equal
   "0.200000000000d+01"
   (letm ((vec (vector-double #(1.0d0 2.0d0 3.0d0))))
     (polynomial-eval vec -1.0d0)))
  (lisp-unit:assert-equal
   '(NIL NIL)
   (multiple-value-list (solve-quadratic 1.0d0 0.0d0 1.0d0)))
  (lisp-unit:assert-equal
   '(1.0d0 1.0d0)
   (multiple-value-list (solve-quadratic 1.0d0 -2.0d0 1.0d0)))
  (lisp-unit:assert-equal
   '(#C(1.0d0 0.0d0) #C(1.0d0 0.0d0))
   (multiple-value-list (solve-quadratic-complex 1.0d0 -2.0d0 1.0d0)))
  (lisp-unit:assert-equal
   '("-0.300000000000d+01" "0.200000000000d+01" "0.700000000000d+01")
   (lisp-unit:fp-values (solve-cubic -6.0d0 -13.0d0 42.0d0)))
  ;; This should use double-float-unequal
  (lisp-unit:assert-equal
   '(("-0.902598308594d-17" "-1.000000000000d+00")
     ("-0.902598308594d-17" "1.000000000000d+00")
     ("0.100000000000d+01" "0.000000000000d+01"))
   (lisp-unit:fp-values (solve-cubic-complex -1.0d0 1.0d0 -1.0d0)))
  (lisp-unit:assert-equal
   '(("-0.809016994375d+00" "0.587785252292d+00")
     ("-0.809016994375d+00" "-0.587785252292d+00")
     ("0.309016994375d+00" "0.951056516295d+00")
     ("0.309016994375d+00" "-0.951056516295d+00")
     ("0.100000000000d+01" "0.000000000000d+01"))
   ;; Example from GSL manual
   (lisp-unit:fp-values
    (polynomial-solve #(-1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0)))))
