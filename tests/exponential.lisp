;; Regression test EXPONENTIAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST EXPONENTIAL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.0025828444588394794d0 18.145581427987647d0
                               12.636598054339759d0 0.5424387252062355d0
                               14.624994234158105d0 7.236607929535993d0
                               0.4345362449683603d0 2.95303920904529d0
                               6.161052939065796d0 3.011686333539114d0
                               2.7451079819355364d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (EXPONENTIAL RNG 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.1d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (EXPONENTIAL-PDF
                                                            0.0d0 10.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.3934693402873666d0)
                        (MULTIPLE-VALUE-LIST (EXPONENTIAL-P 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6065306597126334d0)
                        (MULTIPLE-VALUE-LIST (EXPONENTIAL-Q 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (EXPONENTIAL-PINV
                                                            0.3934693402873666d0
                                                            2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (EXPONENTIAL-QINV
                                                            0.6065306597126334d0
                                                            2.0d0))))

