;; Regression test ELEMENTARY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ELEMENTARY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6.0d0 2.6645352591003757d-15)
                        (MULTIPLE-VALUE-LIST (MULTIPLY 3.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6.0d0 0.5000000000000027d0)
                        (MULTIPLE-VALUE-LIST
                         (MULTIPLY-ERR 3.0d0 0.1d0 2.0d0 0.1d0))))

