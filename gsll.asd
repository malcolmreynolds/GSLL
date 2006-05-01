;********************************************************
; file:        numerica.asd                              
; description: Definition of GSLL system 
; date:        
; author:      Liam Healy
; modified:    Mon May  1 2006 - 15:29
;********************************************************
;;; $Id: $

(asdf:defsystem "gsll"
  :name "gsll"
  :description "GNU Scientific Library for Lisp."
  :version "0"
  :author "Liam M. Healy"
  :licence "GPL"
  :depends-on (cffi cffi-unix)		; http://www.cliki.net/cffi-unix
  :components
  ((:module init
	    :components
	    ((:file "init")
	     (:file "interface" :depends-on (init))
	     ;; http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html
	     (:file "lisp-unit")
	     (:file "tests" :depends-on (lisp-unit))))
   (:module general
	    :depends-on (init)
	    :components
	    ((:file "conditions")
	     (:file "mathematical")))
   ;; complex numbers not necessary?  Just make a struct.
   (:file "cffi-array")
   (:file "polynomial" :depends-on (init cffi-array)) ; see file
   (:module special-functions
	    :depends-on (init)
	    :components
	    ((:file "airy")
	     (:file "bessel")
	     (:file "clausen")
	     (:file "coulomb")
	     (:file "coupling")		; always zero?
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
	     ))
   (:module data
	    :depends-on (init)
	    :components
	    ((:file "data")
	     (:file "block" :depends-on (data))
	     (:file "vector" :depends-on (data))
	     (:file "matrix" :depends-on (data))
	     (:file "permutation" :depends-on (data))
	     (:file "combination" :depends-on (data))
	     (:file "blas1" :depends-on (vector))
	     (:file "blas2" :depends-on (vector matrix))
	     (:file "blas3" :depends-on (blas2 matrix))))
   (:module linear-algebra
	    :depends-on (init data)
	    :components
	    ((:file "lu")
	     (:file "qr")
	     (:file "qrpt")))))
