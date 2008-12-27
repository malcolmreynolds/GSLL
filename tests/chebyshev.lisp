;; Regression test CHEBYSHEV for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST CHEBYSHEV
                       (LISP-UNIT:ASSERT-ERROR 'TYPE-ERROR
                                               (CHEBYSHEV-POINT-EXAMPLE
                                                0.55d0)))

