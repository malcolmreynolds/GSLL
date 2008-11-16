;; Regression test DILOGARITHM for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST DILOGARITHM
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.6449340668482264d0 7.304974700020789d-16)
                        (MULTIPLE-VALUE-LIST (DILOGARITHM 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-0.20561675835602822d0 0.915965594177219d0)
                              #C(2.100180226255977d-15 7.618282373747058d-16))
                        (MULTIPLE-VALUE-LIST (DILOGARITHM #C(0.0d0 1.0d0)))))

