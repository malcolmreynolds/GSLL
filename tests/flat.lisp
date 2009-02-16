;; Regression test FLAT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST FLAT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 1.999741748906672d0 1.1629098753910512d0
                               1.2826178052928299d0 1.9472010820172727d0
                               1.2316565427463502d0 1.4849736143369228d0
                               1.9574769565369934d0 1.7443053431343287d0
                               1.540043658344075d0 1.7399529814720154d0
                               1.7599437981843948d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (FLAT RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (FLAT-PDF 1.2d0
                                                                     1.0d0
                                                                     2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.19999999999999996d0)
                        (MULTIPLE-VALUE-LIST (FLAT-P 1.2d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.8d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (FLAT-Q 1.2d0 1.0d0
                                                                   2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.2d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (FLAT-PINV
                                                            0.19999999999999996d0
                                                            1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.2d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (FLAT-QINV 0.8d0
                                                                      1.0d0
                                                                      2.0d0))))

