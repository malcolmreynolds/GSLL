;********************************************************
; file:        block.lisp                               
; description: Blocks of data                            
; date:        Mon Mar 27 2006 - 12:28                   
; author:      Liam M. Healy                             
; modified:    Wed May 31 2006 - 19:00
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Block definition
(cffi:defcstruct block
  (size :size)
  (data :pointer))

(defdata "block" block-double double-float)
(defdata "block_float" block-single single-float)
(defdata "block_complex" block-complex complex)
(defdata "block_int" block-fixnum fixnum)
