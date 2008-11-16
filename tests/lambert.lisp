;; Regression test LAMBERT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LAMBERT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5671432904097838d0 2.518622157098455d-15)
                        (MULTIPLE-VALUE-LIST (LAMBERT-W0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5671432904097838d0 2.518622157098455d-15)
                        (MULTIPLE-VALUE-LIST (LAMBERT-WM1 1.0d0))))

