;; Regression test ELLIPTIC-INTEGRALS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ELLIPTIC-INTEGRALS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.5707963267948966d0 4.598091522633788d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-K-COMPLETE 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.5707963267948966d0 3.487868498008632d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-E-COMPLETE 0.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.6774175382039307d0 3.008338192795582d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-F -0.5d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.4018194805534952d0 4.232239429377521d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-E -0.5d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.617791316339182d0 3.273131810338189d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-P -0.5d0 2.0d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.06889951441260889d0 3.059753091454848d-17)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-D -0.5d0 2.0d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8813735870195432d0 1.9570424992111216d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-RC 2.0d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7992599630303281d0 1.7747136272346433d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-RD 2.0d0 1.0d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8813735870195432d0 1.9570424992111216d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-RF 2.0d0 1.0d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5228004174989865d0 1.160850121582039d-16)
                        (MULTIPLE-VALUE-LIST
                         (ELLIPTIC-INTEGRAL-RJ 2.0d0 1.0d0 1.0d0 2.0d0))))

