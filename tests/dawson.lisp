;; Regression test DAWSON for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST DAWSON
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5380795069127684d0 1.424354102650492d-15)
                        (MULTIPLE-VALUE-LIST (DAWSON 1.0d0))))

