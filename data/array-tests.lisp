;; Tests of array functions 
;; Liam Healy 2008-10-20 22:41:48EDT array-tests.lisp
;; Time-stamp: <2008-10-20 22:42:46EDT array-tests.lisp>
;; $Id: $

(in-package :gsl)

;;; doesn't work for complex because:
;;; Cannot pass complex scalars to and from GSL functions (structs passed by value).

(generate-all-array-tests
 set-all-m+ :no-complex
 (letm ((v1 (vector-default 3 t))
	(v2 (vector-default 3)))
   (set-all v1 (scalar-default 2))
   (cl-array (m+ v1 v2))))

