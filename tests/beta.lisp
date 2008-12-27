;; Regression test BETA for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST BETA
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.3935474359990073d0 0.7063621551518341d0
                               0.044515648447265056d0 0.09286083229785232d0
                               0.210544366728104d0 0.010114317425185686d0
                               0.4595767375719009d0 0.1515157002550483d0
                               0.1731331145031117d0 0.4270743075655188d0
                               0.3353314142479658d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (BETA-RD RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.8000000000000016d0)
                        (MULTIPLE-VALUE-LIST (BETA-PDF 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.19000000000000017d0)
                        (MULTIPLE-VALUE-LIST (BETA-P 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8099999999999998d0)
                        (MULTIPLE-VALUE-LIST (BETA-Q 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.09999999999999992d0)
                        (MULTIPLE-VALUE-LIST (BETA-PINV 0.19d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.09999999999999988d0)
                        (MULTIPLE-VALUE-LIST (BETA-QINV 0.81d0 1.0d0 2.0d0))))

