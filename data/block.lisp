;********************************************************
; file:        block.lisp                               
; description: Blocks of data                            
; date:        Mon Mar 27 2006 - 12:28                   
; author:      Liam M. Healy                             
; modified:    Mon Mar 27 2006 - 12:34
;********************************************************
;;; $Id: $

(in-package :gsl)

(defclass gsl-block (data)
  ()
  (:documentation "GSL block."))

;;; Block definition
(cffi:defcstruct block
  (size :size)
  (data :pointer))

(gsl-data-functions "block")

