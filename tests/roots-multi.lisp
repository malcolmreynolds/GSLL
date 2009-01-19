;; Regression test ROOTS-MULTI for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ROOTS-MULTI
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.999999999999998d0 0.9999999999999964d0
                              1.9984014443252818d-15 4.440892098500626d-15)
                        (MULTIPLE-VALUE-LIST (ROOTS-MULTI-EXAMPLE-DF NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 1.0d0 0.0d0 0.0d0)
                        (MULTIPLE-VALUE-LIST (ROOTS-MULTI-EXAMPLE NIL))))

