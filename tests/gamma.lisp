;; Regression test GAMMA for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST GAMMA (LISP-UNIT:ASSERT-ERROR 'input-domain (GAMMA -1.0d0))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 120.0d0 2.6645352591003757d-14)
                        (MULTIPLE-VALUE-LIST (GAMMA 6.0d0)))
                       (LISP-UNIT:ASSERT-ERROR 'input-domain (LOG-GAMMA -100.0d0))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 359.13420536957534d0 2.4544868717695813d-13)
                        (MULTIPLE-VALUE-LIST (LOG-GAMMA 100.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 359.13420536957534d0 1.0d0
                              2.4544868717695813d-13)
                        (MULTIPLE-VALUE-LIST (LOG-GAMMA-SIGN 100.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0034780558311105d0 4.456337769159149d-16)
                        (MULTIPLE-VALUE-LIST (GAMMA* 24.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.984126984126984d-4 1.3216940769347103d-19)
                        (MULTIPLE-VALUE-LIST (1/GAMMA 8.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 8.236131750448724d0 -1.184037814936308d0
                              7.315154482555574d-15 2.2665868951123376d-14)
                        (MULTIPLE-VALUE-LIST
                         (LOG-GAMMA-COMPLEX #C(10.0d0 10.0d0))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0011094764610389606d0 2.956239149580215d-18)
                        (MULTIPLE-VALUE-LIST (TAYLOR-COEFFICIENT 12 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 4.790016d8 0.0d0)
                        (MULTIPLE-VALUE-LIST (FACTORIAL 12)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 46080.0d0 0.0d0)
                        (MULTIPLE-VALUE-LIST (DOUBLE-FACTORIAL 12)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 857.9336698258575d0 5.777158772429952d-13)
                        (MULTIPLE-VALUE-LIST (LOG-FACTORIAL 199)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 430.17789358084747d0 1.9103736085528287d-13)
                        (MULTIPLE-VALUE-LIST (LOG-DOUBLE-FACTORIAL 199)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 56.0d0 7.460698725481052d-14)
                        (MULTIPLE-VALUE-LIST (CHOOSE 8 3)))
                       (LISP-UNIT:ASSERT-ERROR 'input-domain (CHOOSE 3 8))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 29.422274169864693d0 1.9338924605168215d-13)
                        (MULTIPLE-VALUE-LIST (LOG-CHOOSE 67 12)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 12.0d0 6.927791673660977d-14)
                        (MULTIPLE-VALUE-LIST (POCHAMMER 3.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 863.2319871924054d0 9.645972767118375d-13)
                        (MULTIPLE-VALUE-LIST (LOG-POCHAMMER 2.0d0 199.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 863.2319871924054d0 1.0d0 9.645972767118375d-13)
                        (MULTIPLE-VALUE-LIST
                         (LOG-POCHAMMER-SIGN 2.0d0 199.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 403199.88888888917d0 3.5998302729212807d-9)
                        (MULTIPLE-VALUE-LIST (RELATIVE-POCHAMMER 2.0d0 9.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.40600584970983766d0 9.568405127077496d-16)
                        (MULTIPLE-VALUE-LIST (INCOMPLETE-GAMMA 2.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5939941502901611d0 3.510166705531295d-15)
                        (MULTIPLE-VALUE-LIST
                         (COMPLEMENTARY-INCOMPLETE-GAMMA 2.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.40600584970983766d0 1.2272947381959672d-15)
                        (MULTIPLE-VALUE-LIST
                         (NONNORMALIZED-INCOMPLETE-GAMMA 2.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.1818181818181818d0 1.0189782228738177d-15)
                        (MULTIPLE-VALUE-LIST (BETA 5.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.7047480922384253d0 3.81791013808375d-15)
                        (MULTIPLE-VALUE-LIST (LOG-BETA 5.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6464466094067263d0 1.0335671699557843d-14)
                        (MULTIPLE-VALUE-LIST
                         (INCOMPLETE-BETA 1.0d0 1.5d0 0.5d0))))

