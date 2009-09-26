;; Regression test QR for GSLL, automatically generated

(in-package :gsl)

;;; Answers inserted from linalg/test.c
;;; GSL has #define GSL_DBL_EPSILON        2.2204460492503131e-16
;;; which is 2x what double-float-epsilon is.
(LISP-UNIT:DEFINE-TEST qrpt
  ;; QRPT solve
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (list *hilb2-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-SOLVE-DIM *HILB2*))))
  (let ((lisp-unit:*epsilon* (* 2 128 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb3-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-SOLVE-DIM *HILB3*))))
  (let ((lisp-unit:*epsilon* (* 2 4096 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb4-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-SOLVE-DIM *HILB4*))))
  (let ((lisp-unit:*epsilon* 0.5d0))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb12-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-SOLVE-DIM *HILB12*))))
  (let ((lisp-unit:*epsilon* (* 2 8 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander2-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-SOLVE-DIM *VANDER2*))))
  (let ((lisp-unit:*epsilon* (* 2 64 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander3-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-SOLVE-DIM *vander3*))))
  (let ((lisp-unit:*epsilon* (* 2 1024 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander4-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-SOLVE-DIM *vander4*))))
  (let ((lisp-unit:*epsilon* 0.05d0))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander12-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-SOLVE-DIM *vander12*))))
  ;; QRPT QRsolve
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (list *hilb2-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-QRSOLVE-DIM *HILB2*))))
  (let ((lisp-unit:*epsilon* (* 2 128 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (list *hilb3-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-QRSOLVE-DIM *HILB3*))))
  (let ((lisp-unit:*epsilon* (* 2 4096 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (list *hilb4-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-QRSOLVE-DIM *HILB4*))))
  (let ((lisp-unit:*epsilon* 0.5d0))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (list *hilb12-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-QRSOLVE-DIM *HILB12*))))
  (let ((lisp-unit:*epsilon* (* 2 8 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander2-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-QRSOLVE-DIM *VANDER2*))))
  (let ((lisp-unit:*epsilon* (* 2 64 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander3-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-QRSOLVE-DIM *vander3*))))
  (let ((lisp-unit:*epsilon* (* 2 1024 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander4-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-QRSOLVE-DIM *vander4*))))
  (let ((lisp-unit:*epsilon* 0.05d0))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander12-soln*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-QRSOLVE-DIM *vander12*))))
  ;; QRPT decomp
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *m35*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *M35*))))
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *m53*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *M53*))))
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *s35*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *s35*))))
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *s53*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *s53*))))
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb2*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *HILB2*))))
  (let ((lisp-unit:*epsilon* (* 2 128 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb3*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *HILB3*))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb4*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *HILB4*))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb12*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *HILB12*))))
  (let ((lisp-unit:*epsilon* (* 2 8 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander2*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *vander2*))))
  (let ((lisp-unit:*epsilon* (* 2 64 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander3*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *vander3*))))
  (let ((lisp-unit:*epsilon* (* 2 1024 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander4*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *vander4*))))
  (let ((lisp-unit:*epsilon* 0.0005d0))	; "FIXME: bad accuracy"
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *vander12*)
     (MULTIPLE-VALUE-LIST (TEST-QRPT-DECOMP-DIM *VANDER12*)))))
