;; Regression test DEBYE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST DEBYE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7775046341122482d0 4.117962028082377d-16)
                        (MULTIPLE-VALUE-LIST (DEBYE-1 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7078784756278294d0 4.948781517277596d-16)
                        (MULTIPLE-VALUE-LIST (DEBYE-2 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6744155640778147d0 5.450931753448871d-16)
                        (MULTIPLE-VALUE-LIST (DEBYE-3 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.654874068886737d0 5.769372522202218d-16)
                        (MULTIPLE-VALUE-LIST (DEBYE-4 1.0d0))))

