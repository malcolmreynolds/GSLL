;********************************************************
; file:        numerica.asd                              
; description: Definition of GSLL system 
; date:        
; author:      Liam Healy
; modified:    Tue Apr 25 2006 - 14:56
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
	     (:file "lisp-unit")
	     (:file "tests")))
   (:module general
	    :depends-on (init)
	    :components
	    ((:file "conditions")
	     (:file "mathematical")))
   ;; complex numbers not necessary?  Just make a struct.
   (:file "cffi-array")
   (:file "polynomial" :depends-on (init cffi-array))	; see file
   (:module data
	    :depends-on (init)
	    :components
	    ((:file "data")
	     (:file "block" :depends-on (data))
	     (:file "vector" :depends-on (data))
	     (:file "matrix" :depends-on (data))
	     (:file "permutation" :depends-on (data))
	     (:file "combination" :depends-on (data))))
   (:module special-functions
	    :depends-on (init)
	    :components
	    ((:file "airy")
	     (:file "bessel")		; one left 
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
	     ))))
