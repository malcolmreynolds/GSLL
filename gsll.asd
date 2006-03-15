;********************************************************
; file:        numerica.asd                              
; description: Definition of GSLL system 
; date:        
; author:      Liam Healy
; modified:    Thu Mar  9 2006 - 23:14
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
   (:file "conditions" :depends-on (init))
   (:file "interface" :depends-on (init))
   (:file "mathematical" :depends-on (init))
   ;; complex numbers not necessary?  Just make a struct.
   (:file "special-functions" :depends-on ("interface"))
   ))
