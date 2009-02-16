;; Regression test POISSON for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST POISSON
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 15 6 9 9 5 8 11 9 11 5 10))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (POISSON RNG 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.11259903214902009d0)
                        (MULTIPLE-VALUE-LIST (POISSON-PDF 8 10.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.3328196787507177d0)
                        (MULTIPLE-VALUE-LIST (POISSON-P 8 10.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6671803212492823d0)
                        (MULTIPLE-VALUE-LIST (POISSON-Q 8 10.0d0))))

