;********************************************************
; file:        numerica.asd                              
; description: Definition of GSLL system 
; date:        
; author:      Liam Healy
; modified:    Fri Mar 17 2006 - 18:52
;********************************************************
;;; $Id: $

(asdf:defsystem "gsll"
  :name "gsll"
  :description "GNU Scientific Library for Lisp."
  :version "0"
  :author "Liam M. Healy"
  :licence "GPL"
  :depends-on (cffi)
  :components
  ((:file "init")
   (:file "interface" :depends-on (init))
   (:module general
	    :depends-on (init interface)
	    :components
	    ((:file "conditions")
	     (:file "mathematical")))
   ;; complex numbers not necessary?  Just make a struct.
   (:module special-functions
	    :depends-on (init interface)
	    :components
	    ((:file "airy")
	     (:file "bessel")
	     (:file "dilogarithm")))))
