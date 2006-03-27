;********************************************************
; file:        numerica.asd                              
; description: Definition of GSLL system 
; date:        
; author:      Liam Healy
; modified:    Sun Mar 26 2006 - 17:02
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
  ((:file "init")
   (:file "interface" :depends-on (init))
   (:module general
	    :depends-on (init interface)
	    :components
	    ((:file "conditions")
	     (:file "mathematical")))
   ;; complex numbers not necessary?  Just make a struct.
   (:file "cffi-array")
   (:file "polynomial" :depends-on (init interface cffi-array)) ; see file
   (:file "data" :depends-on (init))
   (:module special-functions
	    :depends-on (init interface)
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
	     ))))
