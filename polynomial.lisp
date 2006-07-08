;********************************************************
; file:        polynomial.lisp                           
; description: Polynomials                               
; date:        Tue Mar 21 2006 - 18:33                   
; author:      Liam M. Healy                             
; modified:    Fri Jul  7 2006 - 23:11
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Provide autotranslation from CL pure arrays?
;;; Divided differences not complete/tested.

;;;;****************************************************************************
;;;; Polynomial Evaluation
;;;;****************************************************************************

(defun-gsl polynomial-eval (coefficients x)
  "gsl_poly_eval"
  (((gsl-array coefficients) :pointer) ((dim0 coefficients) :size) (x :double))
  :documentation
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
  "Compute a divided-difference representation of the
  interpolating polynomial for the points (@var{xa}, @var{ya}) stored in
  the arrays @var{xa} and @var{ya}.  The output is the
  divided-differences of (@var{xa},@var{ya}) stored in an gsl-vector
  of the same length as xa and ya."
  (let ((len (length xa)))
    (with-data (xad vector-double len)
      (with-data (yad vector-double len)
	(setf (data xad) xa (data yad) ya)
	(divided-difference-int
	 (make-data 'vector-double nil len)
	 xad yad)))))

(defun-gsl polynomial-eval-divided-difference (dd xa x)
  "gsl_poly_dd_eval"
  (((gsl-array dd) :pointer)
   ((gsl-array xa) :pointer)
   ((dim0 xa) :size)
   (x :double))
  :c-return :double
  :documentation
  "Evaluate the polynomial stored in divided-difference form
   in the arrays @var{dd} and @var{xa} at the point @var{x}.")

(defun-gsl taylor-divided-difference (coefs xp dd xa workspace)
  "gsl_poly_dd_taylor"
  (((gsl-array coefs) :pointer)
   (xp :double)
   ((gsl-array dd) :pointer)
   ((gsl-array xa) :pointer)
   ((dim0 xa) :size)
   ((gsl-array workspace) :pointer))
  :invalidate (coefs)
  :documentation
  "Convert the divided-difference representation of a
  polynomial to a Taylor expansion.  The divided-difference representation
  is supplied in the arrays @var{dd} and @var{xa} of the same length.
  On output the Taylor coefficients of the polynomial expanded about the
  point @var{xp} are stored in the array coefs which has the same length
  as xa and dd.  A workspace of length @var{size} must be provided.")

;;;;****************************************************************************
;;;; Quadratic Equations
;;;;****************************************************************************

(defun-gsl solve-quadratic (a b c)
  "gsl_poly_solve_quadratic"
  ((a :double) (b :double) (c :double) (root1 :double) (root2 :double))
  :documentation
  "The real roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if the roots are not real, these
   values are NIL."
  :c-return :number-of-answers)

(defun-gsl solve-quadratic-complex (a b c)
  "gsl_poly_complex_solve_quadratic"
  ((a :double) (b :double) (c :double) (root1 gsl-complex) (root2 gsl-complex))
  :documentation
  "The complex roots of the quadratic equation a x^2 + b x + c = 0.
   Two values are always returned; if a root does not exist, the
   value returned will be NIL."
  :c-return :number-of-answers) 

;;;;****************************************************************************
;;;; Cubic Equations
;;;;****************************************************************************

(defun-gsl solve-cubic (a b c)
  "gsl_poly_solve_cubic"
  ((a :double) (b :double) (c :double)
   (root1 :double) (root2 :double) (root3 :double))
  :documentation
  "Find the real roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  The roots are given
   in ascending order.  Three values are always returned;
   if a root is not real, the value returned for it will be NIL."
  :c-return :number-of-answers)

(defun-gsl solve-cubic-complex (a b c)
  "gsl_poly_complex_solve_cubic"
  ((a :double) (b :double) (c :double)
   (root1 gsl-complex) (root2 gsl-complex) (root3 gsl-complex))
  :documentation
  "Find the complex roots of the cubic equation, x^3 + a x^2 + b x + c = 0
   with a leading coefficient of unity.  Three values are always returned;
   if a root does not exist, the value returned for it will be NIL."
  :c-return :number-of-answers)

;;;;****************************************************************************
;;;; General Polynomial Equations
;;;;****************************************************************************

(defun-gsl complex-workspace-alloc (n)
  "gsl_poly_complex_workspace_alloc" ((n :size))
  :c-return :pointer
  :export nil
  :index with-poly-complex-workspace)

(defun-gsl complex-workspace-free (ws)
  "gsl_poly_complex_workspace_free" ((ws :pointer))
  :c-return :void
  :export nil
  :index with-poly-complex-workspace)

(export '(with-poly-complex-workspace))
(defmacro with-poly-complex-workspace ((workspace size) &body body)
  "Macro to create and cleanup workspace for polynomial root solver." 
  `(let ((,workspace (complex-workspace-alloc ,size)))
     (unwind-protect
	  (progn ,@body)
       (complex-workspace-free ,workspace))))

(defun-gsl polynomial-solve-ws (coefficients workspace answer-pd)
  "gsl_poly_complex_solve"
  (((gsl-array coefficients) :pointer) ((dim0 coefficients) :size)
   (workspace :pointer) ((gsl-array answer-pd) :pointer))
  :return
  ((loop for i from 0 below (dim0 answer-pd) by 2
      collect (complex (gsl-aref answer-pd i)
		       (gsl-aref answer-pd (1+ i)))))
  :documentation
  "Arguments are:
   a GSL array of coefficients, a workspace, a gsl-array of doubles."
  :export nil
  :index polynomial-solve)

(defun polynomial-solve (coefficients)
  "The roots of the general polynomial 
  @math{P(x) = a_0 + a_1 x + a_2 x^2 + ... + a_@{n-1@} x^@{n-1@}} using 
  balanced-QR reduction of the companion matrix.  The parameter @var{n}
  specifies the length of the coefficient array.  The coefficient of the
  highest order term must be non-zero.  The function requires a workspace
  @var{w} of the appropriate size.  The @math{n-1} roots are returned in
  the packed complex array @var{z} of length @math{2(n-1)}, alternating
  real and imaginary parts."
  (let ((len (length coefficients)))
    (with-data (coef vector-double len)
      (setf (data coef) coefficients)
      (with-data (answer vector-double ((* 2 (1- len))))
	(with-poly-complex-workspace (ws len)
	  (values-list (polynomial-solve-ws coef ws answer)))))))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test polynomial
  (lisp-unit:assert-first-fp-equal
   "0.200000000000d+01"
   (with-data (vec vector-double 3)
     (setf (data vec) #(1.0d0 2.0d0 3.0d0))
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
