;; Blocks of data
;; Liam Healy, Mon Mar 27 2006 - 12:28
;; Time-stamp: <2008-03-09 18:54:10EDT block.lisp>
;; $Id$

(in-package :gsl)

;;; Block definition
(cffi:defcstruct block
  (size size)
  (data :pointer))

(add-data-class block double-float block-double-float block "block")
(add-data-class block single-float block-single-float block "block")
(add-data-class block fixnum block-fixnum block "block")
(add-data-class block complex block-complex block "block")

(defdata block double-float)
(defdata block single-float)
(defdata block fixnum)
(defdata block complex)
