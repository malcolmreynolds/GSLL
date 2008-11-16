;; Regression test POWER for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST POWER
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 525.21875d0 9.329759187437503d-13)
                        (MULTIPLE-VALUE-LIST (POW 3.5d0 5))))

