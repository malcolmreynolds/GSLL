;; Regression test LAPLACE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LAPLACE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.005166356198580803d0 -3.942577717493807d0
                               -8.329510281601332d0 1.1159975704649974d0
                               -6.2234038148786555d0 -35.04800398421181d0
                               0.8888158320028845d0 7.161892491969776d0
                               25.24637780914261d0 7.341651048064451d0
                               6.54142651602034d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (sample rng 'laplace :a 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.05d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LAPLACE-PDF 0.0d0
                                                                        10.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6967346701436833d0)
                        (MULTIPLE-VALUE-LIST (LAPLACE-P 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.3032653298563167d0)
                        (MULTIPLE-VALUE-LIST (LAPLACE-Q 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LAPLACE-PINV
                                                            0.6967346701436833d0
                                                            2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LAPLACE-QINV
                                                            0.3032653298563167d0
                                                            2.0d0))))

