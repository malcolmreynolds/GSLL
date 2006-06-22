;********************************************************
; file:        numerica.asd                              
; description: Definition of GSLL system 
; date:        
; author:      Liam Healy
; modified:    Wed Jun 21 2006 - 22:03
;********************************************************
;;; $Id: $

(asdf:defsystem "gsll"
  :name "gsll"
  :description "GNU Scientific Library for Lisp."
  :version "0"
  :author "Liam M. Healy"
  :licence "GPL"
  :depends-on (cffi cffi-unix)	      ; http://www.cliki.net/cffi-unix
  :components
  ((:module init
	    :components
	    ((:file "init")
	     (:file "structures" :depends-on (init))
	     (:file "interface" :depends-on (init))
	     ;; http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html
	     (:file "lisp-unit")
	     (:file "tests" :depends-on (init lisp-unit))))
   (:module general
	    :depends-on (init)
	    :components
	    ((:file "conditions")
	     (:file "mathematical")))
   ;; complex numbers not necessary?  Just make a struct.
   (:module data
	    :depends-on (init)
	    :components
	    ((:file "data")
	     (:file "block" :depends-on (data))
	     (:file "vector" :depends-on (data))
	     (:file "matrix" :depends-on (data vector))
	     (:file "permutation" :depends-on (data vector))
	     (:file "combination" :depends-on (data))))
   #+unnecessary (:file "cffi-array")
   (:file "polynomial" :depends-on (init data))
   (:module special-functions
	    :depends-on (init)
	    :components
	    ((:file "airy")
	     (:file "bessel")
	     (:file "clausen")
	     (:file "coulomb")
	     (:file "coupling")
	     (:file "dawson")
	     (:file "debye")
	     (:file "dilogarithm")
	     (:file "elementary")
	     (:file "elliptic-integrals")
	     (:file "elliptic-functions")
	     (:file "error-functions")
	     (:file "exponential-functions")
	     (:file "exponential-integrals")
	     (:file "fermi-dirac")
	     (:file "gamma")
	     (:file "gegenbauer")
	     (:file "hypergeometric")
	     (:file "laguerre")
	     (:file "lambert")
	     (:file "legendre")
	     (:file "logarithm")
	     (:file "power")
	     (:file "psi")
	     (:file "synchrotron")
	     (:file "transport")
	     (:file "trigonometry")
	     (:file "zeta")))
   (:file "sorting" :depends-on (init data))
   #+future
   (:module linear-algebra
	    :depends-on (init data)
	    :components
	    ((:file "blas1")
	     (:file "blas2")
	     (:file "blas3" :depends-on (blas2))
	     (:file "lu")
	     (:file "qr")
	     (:file "qrpt")
	     (:file "svd")
	     (:file "cholesky")
	     (:file "diagonal")
	     (:file "householder")))
   #+future
   (:file "eigensystems" :depends-on (init data))))
