;; Regression test LOGARITHM for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LOGARITHM
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6931471805599453d0 3.078191837246648d-16)
                        (MULTIPLE-VALUE-LIST (GSL-LOG 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #C(0.34657359027997264d0 0.7853981633974483d0)
                              #C(1.539095918623324d-16 7.69547959311662d-17))
                        (MULTIPLE-VALUE-LIST (GSL-LOG #C(1.0d0 1.0d0))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6931471805599453d0 3.078191837246648d-16)
                        (MULTIPLE-VALUE-LIST (LOG-ABS -2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 9.999500033330834d-5 2.2203350343487824d-20)
                        (MULTIPLE-VALUE-LIST (LOG-1+X 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -4.999666691664667d-9 1.1101490153075193d-24)
                        (MULTIPLE-VALUE-LIST (LOG-1+X-M1 1.d-4))))

