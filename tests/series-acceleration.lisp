;; Regression test SERIES-ACCELERATION for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SERIES-ACCELERATION
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.5961632439130233d0 20 1.5759958390005426d0 13
                              1.6449340669228176d0 8.883604962761638d-11
                              7.459122208786084d-11)
                        (MULTIPLE-VALUE-LIST (ACCELERATION-EXAMPLE NIL))))

