;; Regression test SVD for GSLL, automatically generated

(in-package :gsl)

;;; Answers inserted from linalg/test.c
;;; GSL has #define GSL_DBL_EPSILON        2.2204460492503131e-16
;;; which is 2x what double-float-epsilon is.
(LISP-UNIT:DEFINE-TEST SVD
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST
      (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS '(-8.0d0 18.0d0)))
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM (CREATE-HILBERT-MATRIX 2)))))
  (let ((lisp-unit:*epsilon* (* 2 128 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST
      (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS '(27.0d0 -192.0d0 210.0d0)))
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM (CREATE-HILBERT-MATRIX 3)))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST
      (MAKE-MARRAY 'DOUBLE-FLOAT
		   :INITIAL-CONTENTS '(-64.0d0 900.0d0 -2520.0d0 1820.0d0)))
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM (CREATE-HILBERT-MATRIX 4)))))
  (let ((lisp-unit:*epsilon* 0.5d0))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST
      (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
		   '(-1728.0d0 245388.0d0 -8528520.0d0
		     127026900.0d0 -1009008000.0d0 4768571808.0d0
		     -14202796608.0d0 27336497760.0d0 -33921201600.0d0
		     26189163000.0d0 -11437874448.0d0 2157916488.0d0)))
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM (CREATE-HILBERT-MATRIX 12)))))
  (let ((lisp-unit:*epsilon* (* 2 64 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS '(1.0d0 0.0d0)))
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM (CREATE-VANDERMONDE-MATRIX 2)))))
  (let ((lisp-unit:*epsilon* (* 2 64 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS '(0.0d0 1.0d0 0.0d0)))
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM (CREATE-VANDERMONDE-MATRIX 3)))))
  (let ((lisp-unit:*epsilon* (* 2 1024 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS '(0.0d0 0.0d0 1.0d0  0.0d0)))
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM (CREATE-VANDERMONDE-MATRIX 4)))))
  (let ((lisp-unit:*epsilon* 0.05d0))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST
      (MAKE-MARRAY
       'DOUBLE-FLOAT :INITIAL-CONTENTS
       '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0  0.0d0)))
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM (CREATE-VANDERMONDE-MATRIX 12))))))
