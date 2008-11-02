;; Tests of array functions 
;; Liam Healy 2008-10-20 22:41:48EDT array-tests.lisp
;; Time-stamp: <2008-11-02 18:07:27EST array-tests.lisp>
;; $Id: $

;;; Generate each file with #'write-test-to-file, e.g.
;;; (write-test-to-file 'matrix-set-zero "../test/")

(in-package :gsl)

;;; Some of these don't work for complex because:
;;; Cannot pass complex scalars to and from GSL functions (structs passed by value).

(generate-all-array-tests
 vector-set-all-m+ :no-complex
 (letm ((v1 (array-default 3 t))
	(v2 (array-default 3)))
   (set-all v1 (scalar-default))
   (cl-array (m+ v1 v2))))

(generate-all-array-tests
 matrix-set-all-m+ :no-complex
 (letm ((m1 (array-default '(3 3) t))
	(m2 (array-default '(3 3))))
   (set-all m1 (scalar-default))
   (cl-array (m+ m1 m2))))

(generate-all-array-tests
 vector-set-zero t
 (letm ((v1 (array-default 3)))
   (set-zero v1)
   (cl-array v1)))

(generate-all-array-tests
 matrix-set-zero t
 (letm ((m1 (array-default '(3 3))))
   (set-zero m1)
   (cl-array m1)))

(generate-all-array-tests
 vector-copy t
 (letm ((v1 (array-default 3))
	(v2 (array-default 3 t)))
   (copy v2 v1)
   (cl-array v2)))

(generate-all-array-tests
 matrix-copy t
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3) t)))
   (copy m2 m1)
   (cl-array m2)))

(generate-all-array-tests
 vector-swap t
 (letm ((v1 (array-default 3))
	(v2 (array-default 3)))
   (swap v2 v1)
   (list (cl-array v1) (cl-array v2))))

(generate-all-array-tests
 matrix-swap t
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3))))
   (swap m2 m1)
   (list (cl-array m1) (cl-array m2))))
