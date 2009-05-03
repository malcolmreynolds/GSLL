;; Tests of array functions 
;; Liam Healy 2008-10-20 22:41:48EDT array-tests.lisp
;; Time-stamp: <2009-05-03 16:59:30EDT array-tests.lisp>
;; $Id: $

;;; Generate each file with #'write-test-to-file, e.g.
;;; (write-test-to-file 'matrix-set-zero "../test/")

(in-package :gsl)

;;; Some of these don't work for complex because:
;;; Cannot pass complex scalars to and from GSL functions (structs passed by value).

;;;;****************************************************************************
;;;; Bulk operations
;;;;****************************************************************************

(generate-all-array-tests vector-set-all #+fsbv t #-fsbv :no-complex
 (let ((v1 (array-default 3 t)))
   (cl-array (set-all v1 (scalar-default)))))

(generate-all-array-tests matrix-set-all #+fsbv t #-fsbv :no-complex
 (let ((m1 (array-default '(3 3) t)))
   (cl-array (set-all m1 (scalar-default)))))

(generate-all-array-tests vector-set-zero t
 (let ((v1 (array-default 3)))
   (set-zero v1)
   (cl-array v1)))

(generate-all-array-tests matrix-set-zero t
 (let ((m1 (array-default '(3 3))))
   (set-zero m1)
   (cl-array m1)))

(generate-all-array-tests vector-copy t
 (cl-array (copy (array-default 3))))

(generate-all-array-tests matrix-copy t
 (cl-array (copy (array-default '(3 3)))))

(generate-all-array-tests vector-swap t
 (let ((v1 (array-default 3))
       (v2 (array-default 3)))
   (swap v2 v1)
   (list (cl-array v1) (cl-array v2))))

(generate-all-array-tests matrix-swap t
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3))))
   (swap m2 m1)
   (list (cl-array m1) (cl-array m2))))

;;;;****************************************************************************
;;;; Arithmetic operations
;;;;****************************************************************************

;;; In GSL versions >= 1.12, :no-complex can be changed to T.

(generate-all-array-tests vector-add :no-complex
 (let ((v1 (array-default 3))
       (v2 (array-default 3)))
   (elt+ v1 v2)))

(generate-all-array-tests matrix-add :no-complex
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3))))
   (cl-array (elt+ m1 m2))))

(generate-all-array-tests vector-sub :no-complex
 (let ((v1 (array-default 3))
       (v2 (array-default 3)))
   (cl-array (elt- v1 v2))))

(generate-all-array-tests matrix-sub :no-complex
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3))))
   (cl-array (elt- m1 m2))))

(generate-all-array-tests vector-mult :no-complex
 (let ((v1 (array-default 3))
       (v2 (array-default 3)))
   (cl-array (elt* v1 v2))))

(generate-all-array-tests matrix-mult :no-complex
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3))))
   (cl-array (elt* m1 m2))))

(generate-all-array-tests vector-div :no-complex
 (let ((v1 (array-default 3))
       (v2 (array-default 3)))
   (cl-array (elt/ v1 v2))))

(generate-all-array-tests matrix-div :no-complex
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3))))
   (cl-array (elt/ m1 m2))))

(generate-all-array-tests vector-mult-scalar :no-complex
 (let ((v1 (array-default 3)))
   (cl-array (elt* v1 1.39d0))))

(generate-all-array-tests matrix-mult-scalar :no-complex
 (let ((m1 (array-default '(3 3))))
   (cl-array (elt* m1 1.39d0))))

(generate-all-array-tests vector-add-scalar :no-complex
 (let ((v1 (array-default 3)))
   (cl-array (elt+ v1 18.19d0))))

(generate-all-array-tests matrix-add-scalar :no-complex
 (let ((m1 (array-default '(3 3))))
   (cl-array (elt+ m1 18.19d0))))

;;;;****************************************************************************
;;;; Maximum and minimum elements
;;;;****************************************************************************

(generate-all-array-tests vector-max :no-complex
 (let ((v1 (array-default 3)))
   (mmax v1)))

(generate-all-array-tests matrix-max :no-complex
 (let ((m1 (array-default '(3 3))))
   (mmax m1)))

(generate-all-array-tests vector-min :no-complex
 (let ((v1 (array-default 3)))
   (mmin v1)))

(generate-all-array-tests matrix-min :no-complex
 (let ((m1 (array-default '(3 3))))
   (mmin m1)))

(generate-all-array-tests vector-minmax :no-complex
 (let ((v1 (array-default 3)))
   (minmax v1)))

(generate-all-array-tests matrix-minmax :no-complex
 (let ((m1 (array-default '(3 3))))
   (minmax m1)))

(generate-all-array-tests vector-min-index :no-complex
 (let ((v1 (array-default 8)))
   (min-index v1)))

(generate-all-array-tests matrix-min-index :no-complex
 (let ((m1 (array-default '(3 3))))
   (min-index m1)))

(generate-all-array-tests vector-max-index :no-complex
 (let ((v1 (array-default 8)))
   (max-index v1)))

(generate-all-array-tests matrix-max-index :no-complex
 (let ((m1 (array-default '(3 3))))
   (max-index m1)))

(generate-all-array-tests vector-minmax-index :no-complex
 (let ((v1 (array-default 8)))
   (minmax-index v1)))

(generate-all-array-tests matrix-minmax-index :no-complex
 (let ((m1 (array-default '(3 3))))
   (minmax-index m1)))

;;; No test for mzerop yet.

;;;;****************************************************************************
;;;; Vector-only function definitions
;;;;****************************************************************************

(generate-all-array-tests set-basis t
 (let ((v1 (array-default 8)))
   (set-basis v1 2)
   (cl-array v1)))

(generate-all-array-tests swap-elements t
 (let ((v1 (array-default 8)))
   (swap-elements v1 2 5)
   (cl-array v1)))

(generate-all-array-tests vector-reverse t
 (let ((v1 (array-default 8)))
   (cl-array (vector-reverse v1))))

;;;;****************************************************************************
;;;; Matrix-only function definitions
;;;;****************************************************************************

(generate-all-array-tests set-identity t
 (let ((m1 (array-default '(3 3))))
   (cl-array (set-identity m1))))

(generate-all-array-tests row t
 (let ((m1 (array-default '(3 3)))
       (row (array-default 3 t)))
   (cl-array (row m1 1 row))))

(generate-all-array-tests setf-row t
 (let ((m1 (array-default '(3 3)))
	(row (array-default 3)))
   (setf (row m1 2) row)
   (cl-array m1)))

(generate-all-array-tests column t
 (let ((m1 (array-default '(3 3)))
       (col (array-default 3 t)))
   (cl-array (column m1 1 col))))

(generate-all-array-tests setf-column t
 (let ((m1 (array-default '(3 3)))
	(col (array-default 3)))
   (setf (column m1 2) col)
   (cl-array m1)))

(generate-all-array-tests swap-rows t
 (let ((m1 (array-default '(3 3))))
   (cl-array (swap-rows m1 0 1))))

(generate-all-array-tests swap-columns t
 (let ((m1 (array-default '(3 3))))
   (cl-array (swap-columns m1 1 2))))

(generate-all-array-tests swap-row-column t
 (let ((m1 (array-default '(3 3))))
   (cl-array (swap-row-column m1 0 2))))

(generate-all-array-tests matrix-transpose* t
 (let ((m1 (array-default '(3 3))))
   (cl-array (matrix-transpose* m1))))

(generate-all-array-tests matrix-transpose t
 (let ((m1 (array-default '(3 3)))
       (m2 (array-default '(3 3) t)))
   (cl-array (matrix-transpose m1 m2))))
