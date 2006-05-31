;********************************************************
; file:        library.lisp                              
; description: Load GSL                                  
; date:        Sat Mar  4 2006 - 18:53                   
; author:      Liam M. Healy
; modified:    Fri Mar 24 2006 - 20:04
;********************************************************

(defpackage gsll
  (:nicknames :gsl)
  (:use :common-lisp :cffi))

(cffi:define-foreign-library libgslcblas
  (:unix (:or "/usr/lib/libgslcblas.so.0" "/usr/lib/libgslcblas.so"))
  (t (:default "libgslcblas")))
   
(cffi:use-foreign-library libgslcblas)

(cffi:define-foreign-library libgsl
  (:unix (:or "/usr/lib/libgsl.so.0" "/usr/lib/libgsl.so"))
  (t (:default "libgsl")))
   
(cffi:use-foreign-library libgsl)

;;; If cffi-unix is unavailable, uncomment the following line:
;;; (cffi:defctype :size :unsigned-long)