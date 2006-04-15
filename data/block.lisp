;********************************************************
; file:        block.lisp                               
; description: Blocks of data                            
; date:        Mon Mar 27 2006 - 12:28                   
; author:      Liam M. Healy                             
; modified:    Fri Apr 14 2006 - 19:52
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Block definition
(cffi:defcstruct block
  (size :size)
  (data :pointer))

(defdata "block" :double 'double-float)
