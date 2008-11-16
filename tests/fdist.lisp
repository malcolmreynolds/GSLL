;; Regression test FDIST for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST FDIST
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 18.411308904958695d0 5.120881381831058d0
                               18.104535265974707d0 0.15934280606960227d0
                               0.06272171507636773d0 2.2809441726456456d0
                               0.5259458753395939d0 1.8256109001076744d0
                               0.845346894458977d0 2.5212086970057763d0
                               0.5212415547032052d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 10 COLLECT
                                     (FDIST RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.1594719884624466d0)
                        (MULTIPLE-VALUE-LIST (FDIST-PDF 1.2d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6123724356957948d0)
                        (MULTIPLE-VALUE-LIST (FDIST-P 1.2d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.3876275643042052d0)
                        (MULTIPLE-VALUE-LIST (FDIST-Q 1.2d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.2d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (FDIST-PINV
                                                            0.612372435695795d0
                                                            1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.1999999999999995d0)
                        (MULTIPLE-VALUE-LIST
                         (FDIST-QINV 0.38762756430420503d0 1.0d0 2.0d0))))

