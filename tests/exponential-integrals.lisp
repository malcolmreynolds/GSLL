;; Regression test EXPONENTIAL-INTEGRALS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST EXPONENTIAL-INTEGRALS
                       (LISP-UNIT:ASSERT-ERROR 'INPUT-DOMAIN
                                               (EXPONENTIAL-INTEGRAL-E1 0.0d0))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.21938393439552029d0 2.6541220085226265d-16)
                        (MULTIPLE-VALUE-LIST (EXPONENTIAL-INTEGRAL-E1 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 4.954234356001891d0 7.64289440273947d-15)
                        (MULTIPLE-VALUE-LIST (EXPONENTIAL-INTEGRAL-EI 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.14849550677592205d0 4.130430982976207d-16)
                        (MULTIPLE-VALUE-LIST
                         (EXPONENTIAL-INTEGRAL-EN 2 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.363730673440693d0 4.275641706089105d-15)
                        (MULTIPLE-VALUE-LIST (SHI 1.25d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.217317300914783d0 4.21062110717259d-15)
                        (MULTIPLE-VALUE-LIST (CHI 1.25d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8688926541262327d0 4.083149724141479d-16)
                        (MULTIPLE-VALUE-LIST (EXPONENTIAL-INTEGRAL-3 1.25d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.1464464156732344d0 7.36847290397885d-16)
                        (MULTIPLE-VALUE-LIST (SI 1.25d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4343007240335524d0 1.2207688418479174d-15)
                        (MULTIPLE-VALUE-LIST (CI 1.25d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.103619161676528d0 6.711588415833395d-16)
                        (MULTIPLE-VALUE-LIST (ATANINT 1.25d0))))

