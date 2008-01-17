;; Load GSL
;; Liam Healy Sat Mar  4 2006 - 18:53
;; Time-stamp: <2008-01-16 18:50:15 liam init.lisp>
;; $Id: $

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

;;; The following define :size, from cffi-unix which became cffi-net,
;;; which is apparently turning into something even bigger and more
;;; irrelevant.

(cffi:defctype :uint32 :unsigned-int)
(cffi:defctype :uint64 :unsigned-long)
(cffi:defctype :size
   #-cffi-features:no-long-long :uint64
   #+cffi-features:no-long-long
   #.(progn (cerror "Use :uint32 instead."
   "This platform does not support long long types.")
   :uint32))
