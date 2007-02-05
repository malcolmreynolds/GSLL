;********************************************************
; file:        monte-carlo.lisp                          
; description: Monte Carlo Integration                   
; date:        Sat Feb  3 2007 - 17:42                   
; author:      Liam Healy                                
; modified:    Sun Feb  4 2007 - 16:28
;********************************************************
;;; $Id: $

(in-package :gsl)

(cffi:defcstruct monte-function
  (function :pointer)
  (dimensions :size)
  (parameters :pointer))

(defmacro with-monte-carlo-function
    ((name function number-of-arguments) &body body)
  "Make a function for GSL to integrate."
  ;; Is this creating and deallocating the gsl-function object?
  `(cffi:with-foreign-object (,name 'monte-function)
    (setf (cffi:foreign-slot-value ,name 'monte-function 'function)
     (cffi:get-callback ,function)
     (cffi:foreign-slot-value ,name 'monte-function 'dimensions)
     ,number-of-arguments
     ;; As in numerical-integration, we'll pass the parmeters
     ;; in with a closure.
     (cffi:foreign-slot-value ,name 'monte-function 'parameters)
     (cffi:null-pointer))
    ,@body))

;;; Modify def-gsl-function in numerical-integration.lisp to work with
;;; multivariable arguments.
(export 'def-gsl-function-multi)
(defmacro def-gsl-function-multi (name &body body)
  "Define a GSL function of one C array to be used in
   numerical integration functions.  Arguments are specified
   by (argument n) where n=0, 1,...  Parameters (non
   integration variables) may be passed by using a lexical closure. "
  (let ((arg (gensym "MCARG")))
    `(cffi:defcallback ,name :double
      ((,arg :pointer) (params :pointer))
      (declare (ignore params))
      (macrolet ((argument (n) `(cffi:mem-aref ,',arg :double ,n)))
      	,@body))))

;;;;****************************************************************************
;;;; PLAIN Monte Carlo
;;;;****************************************************************************

(cffi:defcstruct plain-state
  (dim :size)
  (x :pointer))

(defun-gsl monte-carlo-plain-alloc (dim)
  "gsl_monte_plain_alloc"
  ((dim :size))
  :c-return :pointer
  :export nil
  :index with-monte-carlo-plain
  :documentation
  "Allocate and initialize a workspace for Monte Carlo
   integration in @var{dim} dimensions.")

(defun-gsl monte-carlo-plain-init (integrator-state)
  "gsl_monte_plain_init"
  ((integrator-state :pointer))
  :documentation
  "Initialize a previously allocated integration state.
   This allows an existing workspace to be reused for different
   integrations.")

(defun-gsl monte-carlo-plain-free (integrator-state)
  "gsl_monte_plain_free"
  ((integrator-state :pointer))
  :c-return :void
  :export nil
  :index with-monte-carlo-plain
  :documentation
  "Frees the memory associated with the integrator.")

(export 'with-monte-carlo-plain)
(defmacro with-monte-carlo-plain ((workspace size) &body body)
  `(let ((,workspace (monte-carlo-plain-alloc ,size)))
    (unwind-protect
	 (progn
	   ,@body)
      (monte-carlo-plain-free ,workspace))))

(defun-gsl monte-carlo-integrate-plain
    (function lower-limits upper-limits calls generator state)
  "gsl_monte_plain_integrate"
  ((function :pointer)
   ((gsl-array lower-limits) :pointer) ((gsl-array upper-limits) :pointer)
   ((dim0 lower-limits) :size) (calls :size)
   ((generator generator) :pointer)
   (state :pointer)
   (result :double) (abserr :double))
  :documentation
  "Uses the plain Monte Carlo algorithm to integrate the
   function @var{f} over the hypercubic region defined by the
   lower and upper limits in the arrays 'lower-limits and
   'upper-limits, each a gsl-vector of length @var{dim}.
   The integration uses a fixed number
   of function calls @var{calls}, and obtains random sampling points using
   the random number generator 'generator. A previously allocated workspace
   'state must be supplied.  The result of the integration is returned in
   @var{result}, with an estimated absolute error @var{abserr}.")

;;;;****************************************************************************
;;;; MISER
;;;;****************************************************************************

;;; The MISER algorithm of Press and Farrar is based on recursive
;;; stratified sampling.  This technique aims to reduce the overall
;;; integration error by concentrating integration points in the
;;; regions of highest variance.

(cffi:defcstruct miser-state
  (min-calls :size)
  (min-calls-per-bisection :size)
  (dither :double)
  (estimate-frac :double)
  (alpha :double)
  (dim :size)
  (estimate-style :int)
  (depth :int)
  (verbose :int)
  (x :pointer)
  (xmid :pointer)
  (sigma-l :pointer)
  (sigma-r :pointer)
  (fmax-l :pointer)
  (fmax-r :pointer)
  (fmin-l :pointer)
  (fmin-r :pointer)
  (fsum-l :pointer)
  (fsum-r :pointer)
  (fsum2-l :pointer)
  (fsum2-r :pointer)
  (hits-l :pointer)
  (hits-r :pointer))

(defun-gsl monte-carlo-miser-alloc (dim)
  "gsl_monte_miser_alloc"
  ((dim :size))
  :c-return :pointer
  :export nil
  :index with-monte-carlo-miser
  :documentation
  "Allocate and initialize a workspace for Monte Carlo integration in
   @var{dim} dimensions.  The workspace is used to maintain
   the state of the integration.  Returns a pointer to miser-state.")

(defun-gsl monte-carlo-miser-init (integrator-state)
  "gsl_monte_miser_init"
  ((integrator-state :pointer))
  :documentation
  "Initialize a previously allocated integration state.
   This allows an existing workspace to be reused for different
   integrations.")

(defun-gsl monte-carlo-miser-free (integrator-state)
  "gsl_monte_miser_free"
  ((integrator-state :pointer))
  :c-return :void
  :export nil
  :index with-monte-carlo-miser
  :documentation
  "Frees the memory associated with the integrator.")

(export 'with-monte-carlo-miser)
(defmacro with-monte-carlo-miser ((workspace size) &body body)
  "Allocate and use the workspace for the MISER algorithm.
   Parameters can be set by changing values in the structure
   pointed to by the workspace variable."
  `(let ((,workspace (monte-carlo-miser-alloc ,size)))
    (unwind-protect
	 (progn
	   ,@body)
      (monte-carlo-miser-free ,workspace))))

(export 'miser-parameter)
(defmacro miser-parameter (workspace parameter)
  "Get or set with setf the parameter value for the MISER Monte Carlo
   integration method."
  ;; (miser-parameter ws min-calls)
  ;; (setf (miser-parameter ws min-calls) 300)
 `(foreign-slot-value ,workspace 'miser-state ',parameter))

(defun-gsl monte-carlo-integrate-miser
    (function lower-limits upper-limits calls generator state)
  "gsl_monte_miser_integrate"
  ((function :pointer)
   ((gsl-array lower-limits) :pointer) ((gsl-array upper-limits) :pointer)
   ((dim0 lower-limits) :size) (calls :size)
   ((generator generator) :pointer)
   (state :pointer)
   (result :double) (abserr :double))
  :documentation
  "Uses the miser Monte Carlo algorithm to integrate the
   function @var{f} over the hypercubic region defined by the
   lower and upper limits in the arrays 'lower-limits and
   'upper-limits, each a gsl-vector of length @var{dim}.
   The integration uses a fixed number
   of function calls @var{calls}, and obtains random sampling points using
   the random number generator 'generator. A previously allocated workspace
   'state must be supplied.  The result of the integration is returned in
   @var{result}, with an estimated absolute error @var{abserr}.")

;;;;****************************************************************************
;;;; VEGAS
;;;;****************************************************************************

;;; The @sc{vegas} algorithm of Lepage is based on importance sampling.  It
;;; samples points from the probability distribution described by the
;;; function @math{|f|}, so that the points are concentrated in the regions
;;; that make the largest contribution to the integral.

(cffi:defcstruct vegas-state
  ;; grid 
  (dim :size)
  (bins-max :size)
  (bins :uint)				; uint
  (boxes :uint)		       ; these are both counted along the axes
  (xi :pointer)
  (xin :pointer)
  (delx :pointer)
  (weight :pointer)
  (vol :double)
  (x :pointer)
  (bin :pointer)
  (box :pointer)
  (d :pointer)				; distribution
  ;; control variables
  (alpha :double)
  (mode :int)
  (verbose :int)
  (iterations :uint)
  (stage :int)
  ;; scratch variables preserved between calls to vegas1/2/3
  (jac :double)
  (wtd-int-sum :double)
  (sum-wgts :double)
  (chi-sum :double)
  (chisq :double)
  (result :double)
  (sigma :double)
  (it-start :uint)
  (it-num :uint)
  (samples :uint)
  (calls-per-box :uint)
  (ostream :pointer))

(defun-gsl monte-carlo-vegas-alloc (dim)
  "gsl_monte_vegas_alloc"
  ((dim :size))
  :c-return :pointer
  :export nil
  :index with-monte-carlo-vegas
  :documentation
  "Allocate and initialize a workspace for Monte Carlo integration in
   @var{dim} dimensions.  The workspace is used to maintain
   the state of the integration.  Returns a pointer to vegas-state.")

(defun-gsl monte-carlo-vegas-init (integrator-state)
  "gsl_monte_vegas_init"
  ((integrator-state :pointer))
  :documentation
  "Initialize a previously allocated integration state.
   This allows an existing workspace to be reused for different
   integrations.")

(defun-gsl monte-carlo-vegas-free (integrator-state)
  "gsl_monte_vegas_free"
  ((integrator-state :pointer))
  :c-return :void
  :export nil
  :index with-monte-carlo-vegas
  :documentation
  "Frees the memory associated with the integrator.")

(export 'with-monte-carlo-vegas)
(defmacro with-monte-carlo-vegas ((workspace size) &body body)
  "Allocate and use the workspace for the VEGAS algorithm.
   Parameters can be set by changing values in the structure
   pointed to by the workspace variable."
  `(let ((,workspace (monte-carlo-vegas-alloc ,size)))
    (unwind-protect
	 (progn
	   ,@body)
      (monte-carlo-vegas-free ,workspace))))

(export 'vegas-parameter)
(defmacro vegas-parameter (workspace parameter)
  "Get or set with setf the parameter value for the VEGAS Monte Carlo
   integration method."
  ;; (vegas-parameter ws bins-max)
  ;; (setf (vegas-parameter ws bins-max) 300)
 `(foreign-slot-value ,workspace 'vegas-state ',parameter))

(defun-gsl monte-carlo-integrate-vegas
    (function lower-limits upper-limits calls generator state)
  "gsl_monte_vegas_integrate"
  ((function :pointer)
   ((gsl-array lower-limits) :pointer) ((gsl-array upper-limits) :pointer)
   ((dim0 lower-limits) :size) (calls :size)
   ((generator generator) :pointer)
   (state :pointer)
   (result :double) (abserr :double))
  :documentation
  "Uses the @sc{vegas} Monte Carlo algorithm to integrate the
   function @var{f} over the @var{dim}-dimensional hypercubic region
   defined by the lower and upper limits in the arrays @var{xl} and
   @var{xu}, each of size @var{dim}.  The integration uses a fixed number
   of function calls @var{calls}, and obtains random sampling points using
   the random number generator @var{r}. A previously allocated workspace
   @var{s} must be supplied.  The result of the integration is returned in
   @var{result}, with an estimated absolute error @var{abserr}.  The result
   and its error estimate are based on a weighted average of independent
   samples. The chi-squared per degree of freedom for the weighted average
   is returned via the state struct component, @var{s->chisq}, and must be
   consistent with 1 for the weighted average to be reliable.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

;;; Example from Sec. 23.5
;;; This is a function that occurs in random walk studies.

(def-gsl-function-multi monte-carlo-g
  (* (/ (expt pi 3))
     (/ (- 1 (* (cos (argument 0)) (cos (argument 1)) (cos (argument 2)))))))

(defun random-walk-plain-example (&optional (nsamples 500000))
  (with-monte-carlo-plain (ws 3)
    (with-data (lower vector-double 3)
      (with-data (upper vector-double 3)
	(setf (data lower) #(0.0d0 0.0d0 0.0d0)
	      (data upper) (vector pi pi pi))
	(rng-set *rng-mt19937* 0)
	(with-monte-carlo-function (mcf 'monte-carlo-g 3)
	  (monte-carlo-integrate-plain
	   mcf
	   lower upper
	   nsamples
	   *rng-mt19937*
	   ws))))))

(defun random-walk-miser-example (&optional (nsamples 500000))
  (with-monte-carlo-miser (ws 3)
    (with-data (lower vector-double 3)
      (with-data (upper vector-double 3)
	(setf (data lower) #(0.0d0 0.0d0 0.0d0)
	      (data upper) (vector pi pi pi))
	(rng-set *rng-mt19937* 0)
	(with-monte-carlo-function (mcf 'monte-carlo-g 3)
	  (monte-carlo-integrate-miser
	   mcf
	   lower upper
	   nsamples
	   *rng-mt19937*
	   ws))))))

(defun random-walk-vegas-example (&optional (nsamples 500000))
  (with-monte-carlo-vegas (ws 3)
    (with-data (lower vector-double 3)
      (with-data (upper vector-double 3)
	(setf (data lower) #(0.0d0 0.0d0 0.0d0)
	      (data upper) (vector pi pi pi))
	(rng-set *rng-mt19937* 0)
	(with-monte-carlo-function (mcf 'monte-carlo-g 3)
	  (monte-carlo-integrate-vegas
	   mcf
	   lower upper
	   nsamples
	   *rng-mt19937*
	   ws))))))

(lisp-unit:define-test monte-carlo
  (lisp-unit:assert-first-fp-equal
   "0.141220870335d+01"
   (random-walk-plain-example))
  (lisp-unit:assert-first-fp-equal
   "0.138952970588d+01"
   (random-walk-miser-example))
  (lisp-unit:assert-first-fp-equal
   "0.139316327396d+01"
   (random-walk-vegas-example)))
