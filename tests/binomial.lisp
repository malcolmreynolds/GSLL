;; Regression test BINOMIAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST BINOMIAL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 11 3 4 8 4 5 8 6 5 6 6))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (BINOMIAL RNG 0.4d0 12)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.22703033548799986d0)
                        (MULTIPLE-VALUE-LIST (BINOMIAL-PDF 5 0.4d0 12)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6652085575680018d0)
                        (MULTIPLE-VALUE-LIST (BINOMIAL-P 5 0.4d0 12)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.33479144243199815d0)
                        (MULTIPLE-VALUE-LIST (BINOMIAL-Q 5 0.4d0 12))))

