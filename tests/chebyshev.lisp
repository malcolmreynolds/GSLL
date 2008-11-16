;; Regression test CHEBYSHEV for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST CHEBYSHEV
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.7159209901689866d0 -1.5019966658054353d0
                               0.17239719403979925d0))
                        (MULTIPLE-VALUE-LIST (CHEBYSHEV-POINT-EXAMPLE 0.55d0))))

