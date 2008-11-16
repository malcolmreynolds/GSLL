;; Regression test LOGNORMAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LOGNORMAL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 2.386447068127768d0 0.11687602167359055d0
                               4.753374578796263d0 30.093393775755004d0
                               0.8119584375760986d0 3.163421055157545d0
                               0.9146206567715651d0 0.727307901065758d0
                               2.180184852178898d0 3.8908856616896017d0
                               182.18469788916977d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 10 COLLECT
                                     (LOGNORMAL RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.15289833965691607d0)
                        (MULTIPLE-VALUE-LIST
                         (LOGNORMAL-PDF 1.2d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.3413288272347351d0)
                        (MULTIPLE-VALUE-LIST (LOGNORMAL-P 1.2d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6586711727652649d0)
                        (MULTIPLE-VALUE-LIST (LOGNORMAL-Q 1.2d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.2000000000000004d0)
                        (MULTIPLE-VALUE-LIST
                         (LOGNORMAL-PINV 0.3413288272347352d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.2d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LOGNORMAL-QINV
                                                            0.6586711727652649d0
                                                            1.0d0 2.0d0))))

