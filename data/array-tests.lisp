;; Tests of array functions 
;; Liam Healy 2008-10-20 22:41:48EDT array-tests.lisp
;; Time-stamp: <2008-11-02 13:18:30EST array-tests.lisp>
;; $Id: $

(in-package :gsl)

;;; Some of these don't work for complex because:
;;; Cannot pass complex scalars to and from GSL functions (structs passed by value).

(generate-all-array-tests
 set-all-m+ :no-complex
 (letm ((v1 (vector-default 3 t))
	(v2 (vector-default 3)))
   (set-all v1 (scalar-default 2))
   (cl-array (m+ v1 v2))))

(generate-all-array-tests
 set-zero t
 (letm ((v1 (vector-default 3)))
   (set-zero v1)
   (cl-array v1)))

(generate-all-array-tests
 array-copy t
 (letm ((v1 (vector-default 3))
	(v2 (vector-default 3 t)))
   (copy v2 v1)
   (cl-array v2)))

(generate-all-array-tests
 array-swap t
 (letm ((v1 (vector-default 3))
	(v2 (vector-default 3)))
   (swap v2 v1)
   (concatenate 'vector (cl-array v1) (cl-array v2))))
