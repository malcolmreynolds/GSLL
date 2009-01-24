;; Generic functions for optimization
;; Liam Healy 2009-01-03 12:59:07EST generic.lisp
;; Time-stamp: <2009-01-24 17:15:48EST generic.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Generic functions for solve-minimize-fit objects
;;;;****************************************************************************

(defgeneric iterate (object)
  (:documentation "Take the next iteration step for this object."))

(defgeneric solution (object)
  (:documentation
   "The current value of the independent variable(s) that solves this object."))

(defgeneric function-value (object)
  (:documentation
   "The current value of the function that solves this object."))

(defgeneric last-step (object)
  (:documentation			; FDL
   "The last step dx taken by the solver.")) 

;;;;****************************************************************************
;;;; Structures and macros for function definition
;;;;****************************************************************************

(cffi:defcstruct gsl-mfunction
  ;; See /usr/include/gsl/gsl_multiroots.h
  "The definition of a function for multiroot finding in GSL."
  (function :pointer)
  (dimensions sizet)
  (parameters :pointer))

(export 'make-mfunction)
(defmacro make-mfunction (name dimensions)
  "Define a function for multivariate root solving."
  `(make-single-function ,name :success-failure :double gsl-mfunction
    ,dimensions))

(cffi:defcstruct gsl-mfunction-fdf
  ;; See /usr/include/gsl/gsl_multiroots.h
  "The definition of a function and its derivatives for multiroot
   finding in GSL."
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (dimensions sizet)
  (parameters :pointer))

;;;;****************************************************************************
;;;; Function definition
;;;;****************************************************************************

;;; Solvers that require the function and its derivative use the
;;; following structure.
(cffi:defcstruct gsl-function-fdf
  ;; See /usr/include/gsl/gsl_math.h
  "The definition of a function and its derivative for root finding in GSL."
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (parameters :pointer))

(export 'make-solver-functions)

(defmacro make-solver-functions (function df fdf &optional dimensions array)
  "Setup functions for solvers.
   The CL functions name and derivative should be defined previously
   with defuns.  If dimensions is non-nil (positive fixnum), set multiroot solver
   functions.  If dimensions is a number, the functions should expect
   dimensions scalar (double-float) arguments and return "
  (let ((struct (if dimensions 'gsl-mfunction-fdf 'gsl-function-fdf))
	(argtype (if dimensions (if array :pointer `((:double ,dimensions))) :double))
	(rettype (if dimensions :success-failure :double))
	(vecrettype (if array '(:pointer) `((:set :double ,dimensions))))
	(matrettype
	 (if array '(:pointer) `((:set :double ,dimensions ,dimensions)))))
    (with-unique-names (solverfn solverdf solverfdf)
      `(progn
	 (defmcallback
	     ,solverfn ,rettype ,argtype ,(when dimensions vecrettype) ,dimensions
	     ,function)
	 (defmcallback
	     ,solverdf ,rettype ,argtype
	     ,(when dimensions matrettype)
	     ,dimensions ,df)
	 (defmcallback
	     ,solverfdf
	     ,(if dimensions :success-failure :void)
	   ,argtype
	   ,(if dimensions 
		(append vecrettype matrettype)
		'((:set :double 1) (:set :double 1)))
	   ,dimensions ,fdf)
	 (defcbstruct (,solverfn function ,solverdf df ,solverfdf fdf)
	     ,struct
	   ,(when dimensions `((dimensions ,dimensions))))))))

