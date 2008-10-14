;; Load GSL
;; Liam Healy Sat Mar  4 2006 - 18:53
;; Time-stamp: <2008-10-11 14:26:33EDT init.lisp>
;; $Id$

(defpackage gsll
  (:nicknames :gsl)
  (:use :common-lisp :cffi :iterate))

(cffi:define-foreign-library libgslcblas
  (:darwin (:or "/opt/local/lib/libgslcblas.dylib" "/usr/local/lib/libgslcblas.dylib"))
  (:unix (:or "/usr/lib/libgslcblas.so.0" "/usr/lib/libgslcblas.so"))
  (t (:default "libgslcblas")))
   
(cffi:use-foreign-library libgslcblas)

(cffi:define-foreign-library libgsl
  (:darwin (:or "/opt/local/lib/libgsl.dylib" "/usr/local/lib/libgsl.dylib"))
  (:unix (:or "/usr/lib/libgsl.so.0" "/usr/lib/libgsl.so"))
  (t (:default "libgsl")))
   
(cffi:use-foreign-library libgsl)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :native *features*))
