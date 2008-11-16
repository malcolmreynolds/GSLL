;; Regression test SYNCHROTRON for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SYNCHROTRON
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.052827396697912476d0 4.825849878208132d-14)
                        (MULTIPLE-VALUE-LIST (SYNCHROTRON-1 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.04692320582614684d0 6.854449168174663d-14)
                        (MULTIPLE-VALUE-LIST (SYNCHROTRON-2 4.0d0))))

