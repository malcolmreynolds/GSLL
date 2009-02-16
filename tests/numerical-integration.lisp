;; Regression test NUMERICAL-INTEGRATION for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST NUMERICAL-INTEGRATION
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.0d0 2.220446049250313d-14 21)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATION-QNG 'ONE-SINE 0.0d0 PI)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.0d0 2.220446049250313d-14)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATION-QAG 'ONE-SINE 0.0d0 PI :GAUSS15 20)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.0d0 2.220446049250313d-14)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATION-QAG 'ONE-SINE 0.0d0 PI :GAUSS21 40))))

