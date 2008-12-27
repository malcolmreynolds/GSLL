;; Regression test GEGENBAUER for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST GEGENBAUER
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6.0d0 5.329070518200751d-15)
                        (MULTIPLE-VALUE-LIST (GEGENBAUER-1 1.0d0 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 35.0d0 1.5765166949677223d-14)
                        (MULTIPLE-VALUE-LIST (GEGENBAUER-2 1.0d0 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 204.0d0 9.126033262418787d-14)
                        (MULTIPLE-VALUE-LIST (GEGENBAUER-3 1.0d0 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1189.0d0 1.056044141023449d-12)
                        (MULTIPLE-VALUE-LIST (GEGENBAUER 4 1.0d0 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(1.0d0 6.0d0 35.0d0 204.0d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((ARR (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (GEGENBAUER-ARRAY 1.0d0 3.0d0 ARR)
                           (CL-ARRAY ARR)))))

