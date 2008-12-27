;; Regression test GAMMA-RANDIST for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST GAMMA-RANDIST
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 3.012983063768798d0 2.216796987787054d0
                               5.033971231985026d0 0.6152002566487763d0
                               0.1876159751197978d0 1.7884608326846099d0
                               0.30812625873110316d0 1.1328459017528132d0
                               0.7363931539298727d0 0.9843618987581162d0
                               0.06871686155296197d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (GAMMA-RD RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 3.012983063768798d0 2.216796987787054d0
                               5.033971231985026d0 0.6152002566487763d0
                               0.1876159751197978d0 1.7884608326846099d0
                               0.30812625873110316d0 1.1328459017528132d0
                               0.7363931539298727d0 0.9843618987581162d0
                               0.06871686155296197d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (GAMMA-MT RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.475614712250357d0)
                        (MULTIPLE-VALUE-LIST (GAMMA-PDF 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.04877057549928599d0)
                        (MULTIPLE-VALUE-LIST (GAMMA-P 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.951229424500714d0)
                        (MULTIPLE-VALUE-LIST (GAMMA-Q 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.10000000000000006d0)
                        (MULTIPLE-VALUE-LIST
                         (GAMMA-PINV 0.048770575499286005d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.10000000000000003d0)
                        (MULTIPLE-VALUE-LIST
                         (GAMMA-QINV 0.951229424500714d0 1.0d0 2.0d0))))

