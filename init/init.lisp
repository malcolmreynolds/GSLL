;; Load GSL
;; Liam Healy Sat Mar  4 2006 - 18:53
;; Time-stamp: <2009-01-26 21:39:39EST init.lisp>
;; $Id$

(defpackage gsll
  (:nicknames :gsl)
  (:use :common-lisp :cffi))

(cffi:define-foreign-library libgslcblas
    (:darwin
     (:or "/opt/local/lib/libgslcblas.dylib" "/sw/lib/libgslcblas.dylib"
	  "/usr/local/lib/libgslcblas.dylib"))
  (:cygwin (:or "/bin/cyggslcblas-0.dll"))
  (:unix (:or "/usr/lib/libgslcblas.so.0" "/usr/lib/libgslcblas.so"
	      "/usr/lib64/libgslcblas.so.0" "/usr/lib64/libgslcblas.so"))
  (t (:default "libgslcblas")))
   
(cffi:use-foreign-library libgslcblas)

;; When calling libgsl from emacs built for windows and slime, and
;; using clisp built for cygwin, we have to load lapack/cygblas.dll
;; before loading cyggsl-0.dll
#+(and clisp cygwin)
(cffi:load-foreign-library "/lib/lapack/cygblas.dll")

(cffi:define-foreign-library libgsl
    (:darwin
     (:or "/opt/local/lib/libgsl.dylib" "/sw/lib/libgsl.dylib"
	  "/usr/local/lib/libgsl.dylib"))
  (:cygwin (:or "/bin/cyggsl-0.dll"))
  (:unix (:or "/usr/lib/libgsl.so.0" "/usr/lib/libgsl.so"
	      "/usr/lib64/libgsl.so.0" "/usr/lib64/libgsl.so"))
  (t (:default "libgsl")))
   
(cffi:use-foreign-library libgsl)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :native *features*))

#+openmcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :callback-toplevel-only *features*))

;;; CFFI currently doesn't allow uninterned callback names for CCL;
;;; patch here.

#-openmcl (import 'cl-utilities:with-unique-names :gsll)

#+openmcl
(defmacro gsll::with-unique-names ((&rest bindings) &body body)
  `(cl-utilities:with-unique-names
       (,@bindings)
     (let ,(mapcar (lambda (symb) `(,symb (intern (symbol-name ,symb))))
		   bindings)
       ,@body)))
