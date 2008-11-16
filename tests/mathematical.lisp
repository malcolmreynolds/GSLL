;; Regression test MATHEMATICAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATHEMATICAL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 9.995003330835331d-4)
                        (MULTIPLE-VALUE-LIST (LOG+1 0.001d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0010005001667083417d0)
                        (MULTIPLE-VALUE-LIST (EXP-1 0.001d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 5.0d0 2.220446049250313d-15)
                        (MULTIPLE-VALUE-LIST (HYPOTENUSE 3.0d0 4.0d0))))

