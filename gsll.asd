;; Definition of GSLL system 
;; Liam Healy
;; Time-stamp: <2009-01-14 22:12:10EST gsll.asd>
;; $Id$

(asdf:defsystem "gsll"
  :name "gsll"
  :description "GNU Scientific Library for Lisp."
  :version "0"
  :author "Liam M. Healy"
  :licence "LLGPL v3, FDL"
  :depends-on (cffi trivial-garbage cl-utilities)
  :components
  ((:module init
	    :components
	    ((:file "init")
	     (:file "conditions" :depends-on (init))
	     (:file "mobject" :depends-on (init))
	     (:file "types" :depends-on (init))
	     (:file "complex-types" :depends-on (types))
	     (:file "element-types" :depends-on (init complex-types))
	     (:file "number-conversion" :depends-on (init))
	     (:file "interface"
		    :depends-on (init conditions element-types number-conversion))
	     (:file "defmfun" :depends-on (init element-types interface))
	     (:file "defmfun-array" :depends-on (defmfun))
	     (:file "defmfun-single" :depends-on (defmfun))
	     (:file "callback" :depends-on (init))
	     (:file "generate-examples" :depends-on (init))))
   (:module floating-point
	    :depends-on (init)
	    :components
	    ((:file "ieee-modes")
	     (:file "floating-point")))
   (:file "mathematical" :depends-on (init))
   ;; complex numbers not necessary?  Just make a struct.
   (:module data
	    :depends-on (init)
	    :components
	    ((:file "foreign-friendly")
	     (:file "foreign-array" :depends-on (foreign-friendly))
	     (:file "marray" :depends-on (foreign-array))
	     (:file "vector" :depends-on (marray))
	     (:file "matrix" :depends-on (marray vector))
	     (:file "maref" :depends-on (marray vector matrix))
	     (:file "both" :depends-on (marray vector matrix))
	     (:file "array-tests" :depends-on (both))
	     (:file "permutation" :depends-on (marray))
	     (:file "combination" :depends-on (marray))))
   (:file "polynomial" :depends-on (init data))
   (:module special-functions
	    :depends-on (init)
	    :components
	    ((:file "return-structures")
	     (:file "airy" :depends-on (return-structures))
	     (:file "bessel" :depends-on (return-structures))
	     (:file "clausen" :depends-on (return-structures))
	     (:file "coulomb" :depends-on (return-structures))
	     (:file "coupling" :depends-on (return-structures))
	     (:file "dawson" :depends-on (return-structures))
	     (:file "debye" :depends-on (return-structures))
	     (:file "dilogarithm" :depends-on (return-structures))
	     (:file "elementary" :depends-on (return-structures))
	     (:file "elliptic-integrals" :depends-on (return-structures))
	     (:file "elliptic-functions" :depends-on (return-structures))
	     (:file "error-functions" :depends-on (return-structures))
	     (:file "exponential-functions" :depends-on (return-structures))
	     (:file "exponential-integrals" :depends-on (return-structures))
	     (:file "fermi-dirac" :depends-on (return-structures))
	     (:file "gamma" :depends-on (return-structures))
	     (:file "gegenbauer" :depends-on (return-structures))
	     (:file "hypergeometric" :depends-on (return-structures))
	     (:file "laguerre" :depends-on (return-structures))
	     (:file "lambert" :depends-on (return-structures))
	     (:file "legendre" :depends-on (return-structures))
	     (:file "logarithm" :depends-on (return-structures))
	     (:file "power" :depends-on (return-structures))
	     (:file "psi" :depends-on (return-structures))
	     (:file "synchrotron" :depends-on (return-structures))
	     (:file "transport" :depends-on (return-structures))
	     (:file "trigonometry" :depends-on (return-structures))
	     (:file "zeta" :depends-on (return-structures))))
   (:file "sorting" :depends-on (init data))
   (:module linear-algebra
	    :depends-on (init data special-functions)
	    :components
	    ((:file "blas1")
	     (:file "blas2")
	     (:file "blas3" :depends-on (blas2))
	     (:file "exponential")
	     (:file "lu")
	     (:file "qr")
	     (:file "qrpt")
	     (:file "svd")
	     (:file "cholesky")
	     (:file "diagonal")
	     (:file "householder")))
   (:file "eigensystems" :depends-on (init data))
   ;; Skip fft for now, I'm not sure how it works in C
   (:file "numerical-integration" :depends-on (init))
   (:module random
	    :depends-on (init data)
	    :components
	    ((:file "rng-types")
	     (:file "generators" :depends-on (rng-types))
	     (:file "quasi" :depends-on (rng-types generators))
	     (:file "gaussian" :depends-on (rng-types))
	     (:file "gaussian-tail" :depends-on (rng-types))
	     (:file "gaussian-bivariate" :depends-on (rng-types))
	     (:file "exponential" :depends-on (rng-types))
	     (:file "laplace" :depends-on (rng-types))
	     (:file "exponential-power" :depends-on (rng-types))
	     (:file "cauchy" :depends-on (rng-types))
	     (:file "rayleigh" :depends-on (rng-types))
	     (:file "rayleigh-tail" :depends-on (rng-types))
	     (:file "landau" :depends-on (rng-types))
	     (:file "levy" :depends-on (rng-types))
	     (:file "gamma" :depends-on (rng-types))
	     (:file "flat" :depends-on (rng-types))
	     (:file "lognormal" :depends-on (rng-types))
	     (:file "chi-squared" :depends-on (rng-types))
	     (:file "fdist" :depends-on (rng-types))
	     (:file "tdist" :depends-on (rng-types))
	     (:file "beta" :depends-on (rng-types))
	     (:file "logistic" :depends-on (rng-types))
	     (:file "pareto" :depends-on (rng-types))
	     (:file "spherical-vector" :depends-on (rng-types))
	     (:file "weibull" :depends-on (rng-types))
	     (:file "gumbel1" :depends-on (rng-types))
	     (:file "gumbel2" :depends-on (rng-types))
	     (:file "dirichlet" :depends-on (rng-types))
	     (:file "discrete" :depends-on (rng-types))
	     (:file "poisson" :depends-on (rng-types))
	     (:file "bernoulli" :depends-on (rng-types))
	     (:file "binomial" :depends-on (rng-types))
	     (:file "multinomial" :depends-on (rng-types))
	     (:file "negative-binomial" :depends-on (rng-types))
	     (:file "geometric" :depends-on (rng-types))
	     (:file "hypergeometric" :depends-on (rng-types))
	     (:file "logarithmic" :depends-on (rng-types))
	     (:file "shuffling-sampling" :depends-on (rng-types))))
   (:module statistics
	    :depends-on (init data)
	    :components
	    ((:file "mean-variance")
	     (:file "absolute-deviation")
	     (:file "higher-moments")
	     (:file "autocorrelation")
	     (:file "covariance")
	     ;; minimum and maximum values provided in vector.lisp
	     (:file "median-percentile")))
   (:module histogram
	    :depends-on (init linear-algebra)
	    :components
	    ((:file "histogram")
	     (:file "updating-accessing" :depends-on (histogram))
	     (:file "statistics" :depends-on (histogram))
	     (:file "operations" :depends-on (histogram))
	     (:file "probability-distribution" :depends-on (histogram))
	     (:file "ntuple")))
   (:file "monte-carlo" :depends-on (init data random))
   (:module ordinary-differential-equations
	    :depends-on (init)
	    :components
	    ((:file "ode-system")
	     (:file "stepping")
	     (:file "control")
	     (:file "evolution")
	     (:file "ode-example" :depends-on (ode-system stepping))))
   (:module interpolation
	    :depends-on (init)
	    :components
	    ((:file "interpolation")
	     (:file "types" :depends-on (interpolation))
	     (:file "lookup")
	     (:file "evaluation")
	     (:file "spline-example" :depends-on (types))))
   (:file "numerical-differentiation" :depends-on (init))
   (:file "chebyshev" :depends-on (init))
   (:file "series-acceleration" :depends-on (init))
   (:file "wavelet" :depends-on (init data))
   (:file "hankel" :depends-on (init data))
   (:module solve-minimize-fit
	    :depends-on (init data random)
	    :components
	    ((:file "generic")
	     (:file "roots-one" :depends-on (generic))
	     (:file "minimization-one" :depends-on (generic))
	     (:file "roots-multi" :depends-on (roots-one generic))
	     (:file "minimization-multi" :depends-on (generic))
	     (:file "linear-least-squares")
	     (:file "nonlinear-least-squares" :depends-on (generic))))
   (:file "basis-splines" :depends-on (init data))))
