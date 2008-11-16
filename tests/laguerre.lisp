;; Regression test LAGUERRE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LAGUERRE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.0d0 2.220446049250313d-15)
                        (MULTIPLE-VALUE-LIST (LAGUERRE-1 1.0d0 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.5d0 1.7985612998927536d-14)
                        (MULTIPLE-VALUE-LIST (LAGUERRE-2 1.0d0 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.5d0 6.59472476627343d-14)
                        (MULTIPLE-VALUE-LIST (LAGUERRE-3 1.0d0 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.875d0 6.793349802129357d-14)
                        (MULTIPLE-VALUE-LIST (LAGUERRE 4 1.0d0 3.0d0))))

