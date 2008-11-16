;; Regression test CLAUSEN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST CLAUSEN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4335982032355329d0 1.2032502825912828d-15)
                        (MULTIPLE-VALUE-LIST (CLAUSEN 2.5d0))))

