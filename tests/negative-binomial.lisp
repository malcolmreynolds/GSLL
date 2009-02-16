;; Regression test NEGATIVE-BINOMIAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST NEGATIVE-BINOMIAL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 15 21 19 15 8 18 23 18 33 16 10))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (NEGATIVE-BINOMIAL RNG 0.4d0 12.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0056984767089869d0)
                        (MULTIPLE-VALUE-LIST
                         (NEGATIVE-BINOMIAL-PDF 5 0.4d0 12.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.010594202555514881d0)
                        (MULTIPLE-VALUE-LIST
                         (NEGATIVE-BINOMIAL-P 5 0.4d0 12.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9894057974444851d0)
                        (MULTIPLE-VALUE-LIST
                         (NEGATIVE-BINOMIAL-Q 5 0.4d0 12.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 15 21 19 15 8 18 23 18 33 16 10))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (PASCAL RNG 0.4d0 12)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0056984767089869d0)
                        (MULTIPLE-VALUE-LIST (PASCAL-PDF 5 0.4d0 12)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.010594202555514881d0)
                        (MULTIPLE-VALUE-LIST (PASCAL-P 5 0.4d0 12)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9894057974444851d0)
                        (MULTIPLE-VALUE-LIST (PASCAL-Q 5 0.4d0 12))))

