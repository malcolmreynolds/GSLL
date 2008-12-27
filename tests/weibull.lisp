;; Regression test WEIBULL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST WEIBULL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.0160712303786595d0 1.347055359960668d0
                               1.1241262408795445d0 0.2329031397826649d0
                               1.209338423856536d0 0.8506825453443836d0
                               0.20845532973957762d0 0.5434187344070215d0
                               0.7849237503774361d0 0.5487883320132739d0
                               0.5239377808419179d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (WEIBULL RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.24263174972226745d0)
                        (MULTIPLE-VALUE-LIST (WEIBULL-PDF 1.5d0 1.3d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9992887742799077d0)
                        (MULTIPLE-VALUE-LIST (WEIBULL-P 3.5d0 1.3d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 7.112257200923508d-4)
                        (MULTIPLE-VALUE-LIST (WEIBULL-Q 3.5d0 1.3d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 3.5000000000000164d0)
                        (MULTIPLE-VALUE-LIST
                         (WEIBULL-PINV 0.9992887742799077d0 1.3d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 3.5d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (WEIBULL-QINV
                                                            7.112257200923508d-4
                                                            1.3d0 2.0d0))))

