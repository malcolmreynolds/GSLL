;; Regression test LU for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LU
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-1.9999999999999998d0 1.0d0)
                             (1.4999999999999998d0 -0.49999999999999994d0)))
                        (MULTIPLE-VALUE-LIST
                         (CL-ARRAY
                          (INVERT-MATRIX
                           (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '(2 2)
                                        :INITIAL-CONTENTS
                                        '(1.0d0 2.0d0 3.0d0 4.0d0)))))))

