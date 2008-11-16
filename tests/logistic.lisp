;; Regression test LOGISTIC for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LOGISTIC
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 82.6131993192451d0 -16.367346042668906d0
                               -9.31513272043762d0 28.87020708710654d0
                               -11.989809875784625d0 -0.6012364762000397d0
                               31.142555263622d0 10.684673721048895d0
                               1.6051840954024446d0 10.457241904701199d0
                               11.523714106294097d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 10 COLLECT
                                     (LOGISTIC RNG 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.2350037122015945d0)
                        (MULTIPLE-VALUE-LIST (LOGISTIC-PDF 0.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6224593312018546d0)
                        (MULTIPLE-VALUE-LIST (LOGISTIC-P 0.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.37754066879814546d0)
                        (MULTIPLE-VALUE-LIST (LOGISTIC-Q 0.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5000000000000001d0)
                        (MULTIPLE-VALUE-LIST
                         (LOGISTIC-PINV 0.6224593312018546d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4999999999999998d0)
                        (MULTIPLE-VALUE-LIST
                         (LOGISTIC-QINV 0.37754066879814546d0 1.0d0))))

