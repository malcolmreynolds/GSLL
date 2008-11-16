;; Regression test COUPLING for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST COUPLING
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7071067811865475d0 3.14018491736755d-16)
                        (MULTIPLE-VALUE-LIST (COUPLING-3J 0 1 1 0 1 -1)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.408248290463863d0 5.438959822042073d-16)
                        (MULTIPLE-VALUE-LIST (COUPLING-6J 1 1 2 0 2 1)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.1388888888888889d0 6.638400825147663d-16)
                        (MULTIPLE-VALUE-LIST (COUPLING-9J 1 1 2 1 2 1 2 1 1))))

