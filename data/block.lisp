;; Blocks of data
;; Liam Healy, Mon Mar 27 2006 - 12:28
;; Time-stamp: <2008-02-17 09:18:05EST block.lisp>
;; $Id: $

(in-package :gsl)

;;; Block definition
(cffi:defcstruct block
  (size size)
  (data :pointer))

(defdata "block" block-double double-float)
(defdata "block_float" block-single single-float)
(defdata "block_complex" block-complex complex)
(defdata "block_int" block-fixnum fixnum)
