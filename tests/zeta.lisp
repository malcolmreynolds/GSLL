;; Regression test ZETA for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ZETA
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.6449340668482264d0 7.304974700020789d-16)
                        (MULTIPLE-VALUE-LIST (ZETA 2)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.612375348685493d0 1.419471177334903d-14)
                        (MULTIPLE-VALUE-LIST (ZETA 1.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6449340668482264d0 2.8640826015201633d-16)
                        (MULTIPLE-VALUE-LIST (ZETA-1 2)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.612375348685493d0 1.419471177334903d-14)
                        (MULTIPLE-VALUE-LIST (ZETA-1 1.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.4037797688568256d0 8.104244828616706d-15)
                        (MULTIPLE-VALUE-LIST (HURWITZ-ZETA 1.5d0 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7651470246254092d0 1.0661276646941275d-14)
                        (MULTIPLE-VALUE-LIST (ETA 1.5d0))))

