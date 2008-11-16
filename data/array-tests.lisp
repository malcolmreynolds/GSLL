;; Tests of array functions 
;; Liam Healy 2008-10-20 22:41:48EDT array-tests.lisp
;; Time-stamp: <2008-11-16 10:28:21EST array-tests.lisp>
;; $Id: $

;;; Generate each file with #'write-test-to-file, e.g.
;;; (write-test-to-file 'matrix-set-zero "../test/")

(in-package :gsl)

;;; Some of these don't work for complex because:
;;; Cannot pass complex scalars to and from GSL functions (structs passed by value).

;;;;****************************************************************************
;;;; Bulk operations
;;;;****************************************************************************

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

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

(generate-all-array-tests vector-m+ :no-complex
 (letm ((v1 (array-default 3))
	(v2 (array-default 3)))
   (cl-array (m+ v1 v2))))

(generate-all-array-tests matrix-m+ :no-complex
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3))))
   (cl-array (m+ m1 m2))))

(generate-all-array-tests vector-m- :no-complex
 (letm ((v1 (array-default 3))
	(v2 (array-default 3)))
   (cl-array (m- v1 v2))))

(generate-all-array-tests matrix-m- :no-complex
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3))))
   (cl-array (m- m1 m2))))

(generate-all-array-tests vector-mult :no-complex
 (letm ((v1 (array-default 3))
	(v2 (array-default 3)))
   (cl-array (m* v1 v2))))

(generate-all-array-tests matrix-mult :no-complex
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3))))
   (cl-array (m* m1 m2))))

(generate-all-array-tests vector-div :no-complex
 (letm ((v1 (array-default 3))
	(v2 (array-default 3)))
   (cl-array (m/ v1 v2))))

(generate-all-array-tests matrix-div :no-complex
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3))))
   (cl-array (m/ m1 m2))))

(generate-all-array-tests vector-mult-scalar :no-complex
 (letm ((v1 (array-default 3)))
   (cl-array (m*c v1 1.39d0))))

(generate-all-array-tests matrix-mult-scalar :no-complex
 (letm ((m1 (array-default '(3 3))))
   (cl-array (m*c m1 1.39d0))))

(generate-all-array-tests vector-add-scalar :no-complex
 (letm ((v1 (array-default 3)))
   (cl-array (m+c v1 18.19d0))))

(generate-all-array-tests matrix-add-scalar :no-complex
 (letm ((m1 (array-default '(3 3))))
   (cl-array (m+c m1 18.19d0))))

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(generate-all-array-tests vector-max :no-complex
 (letm ((v1 (array-default 3)))
   (mmax v1)))

(generate-all-array-tests matrix-max :no-complex
 (letm ((m1 (array-default '(3 3))))
   (mmax m1)))

#| Temporarily commented out twos-complement answer for SIGNED-BYTE-8 and SIGNED-BYTE-16
(generate-all-array-tests vector-min :no-complex
 (letm ((v1 (array-default 3)))
   (mmin v1)))

(generate-all-array-tests matrix-min :no-complex
 (letm ((m1 (array-default '(3 3))))
   (mmin m1)))
|#

(generate-all-array-tests vector-minmax :no-complex
 (letm ((v1 (array-default 3)))
   (minmax v1)))

(generate-all-array-tests matrix-minmax :no-complex
 (letm ((m1 (array-default '(3 3))))
   (minmax m1)))

(generate-all-array-tests vector-min-index :no-complex
 (letm ((v1 (array-default 8)))
   (min-index v1)))

(generate-all-array-tests matrix-min-index :no-complex
 (letm ((m1 (array-default '(3 3))))
   (min-index m1)))

(generate-all-array-tests vector-max-index :no-complex
 (letm ((v1 (array-default 8)))
   (max-index v1)))

(generate-all-array-tests matrix-max-index :no-complex
 (letm ((m1 (array-default '(3 3))))
   (max-index m1)))

(generate-all-array-tests vector-minmax-index :no-complex
 (letm ((v1 (array-default 8)))
   (minmax-index v1)))

(generate-all-array-tests matrix-minmax-index :no-complex
 (letm ((m1 (array-default '(3 3))))
   (minmax-index m1)))

;;; No test for mzerop yet.

;;;;****************************************************************************
;;;; Vector-only function definitions
;;;;****************************************************************************

(generate-all-array-tests set-basis t
 (letm ((v1 (array-default 8)))
   (set-basis v1 2)
   (cl-array v1)))

(generate-all-array-tests swap-elements t
 (letm ((v1 (array-default 8)))
   (swap-elements v1 2 5)
   (cl-array v1)))

(generate-all-array-tests vector-reverse t
 (letm ((v1 (array-default 8)))
   (cl-array (vector-reverse v1))))

;;;;****************************************************************************
;;;; Matrix-only function definitions
;;;;****************************************************************************

(generate-all-array-tests set-identity t
 (letm ((m1 (array-default '(3 3))))
   (cl-array (set-identity m1))))

(generate-all-array-tests row t
 (letm ((m1 (array-default '(3 3)))
	(row (array-default 3 t)))
   (cl-array (row row m1 1))))

(generate-all-array-tests setf-row t
 (letm ((m1 (array-default '(3 3)))
	(row (array-default 3)))
   (setf (row m1 2) row)
   (cl-array m1)))

(generate-all-array-tests column t
 (letm ((m1 (array-default '(3 3)))
	(col (array-default 3 t)))
   (cl-array (column col m1 1))))

(generate-all-array-tests setf-column t
 (letm ((m1 (array-default '(3 3)))
	(col (array-default 3)))
   (setf (column m1 2) col)
   (cl-array m1)))

(generate-all-array-tests swap-rows t
 (letm ((m1 (array-default '(3 3))))
   (cl-array (swap-rows m1 0 1))))

(generate-all-array-tests swap-columns t
 (letm ((m1 (array-default '(3 3))))
   (cl-array (swap-columns m1 1 2))))

(generate-all-array-tests swap-row-column t
 (letm ((m1 (array-default '(3 3))))
   (cl-array (swap-row-column m1 0 2))))

(generate-all-array-tests matrix-transpose t
 (letm ((m1 (array-default '(3 3))))
   (cl-array (matrix-transpose m1))))

(generate-all-array-tests matrix-transpose-copy t
 (letm ((m1 (array-default '(3 3)))
	(m2 (array-default '(3 3) t)))
   (cl-array (matrix-transpose-copy m2 m1))))
