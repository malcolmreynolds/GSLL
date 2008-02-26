;; Load GSL
;; Liam Healy Sat Mar  4 2006 - 18:53
;; Time-stamp: <2008-02-25 19:31:49EST init.lisp>
;; $Id$

(defpackage gsll
  (:nicknames :gsl)
  (:use :common-lisp :cffi))

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

;;; The following define :size, from cffi-unix which became cffi-net,
;;; which is apparently turning into something even bigger and more
;;; irrelevant.  All we need is the definition of :size.  This is
;;; not in the keyword package to suppress warnings from CFFI.

(cffi:defctype uint32 :unsigned-int)
(cffi:defctype uint64 :unsigned-long)
(cffi:defctype gsll::size
   #-cffi-features:no-long-long uint64
   #+cffi-features:no-long-long
   #.(progn (cerror "Use uint32 instead."
   "This platform does not support long long types.")
   uint32))
