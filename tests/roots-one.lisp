;; Regression test ROOTS-ONE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ROOTS-ONE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.236067977499978d0)
                        (MULTIPLE-VALUE-LIST (ROOTS-ONE-FDF-EXAMPLE NIL))))

