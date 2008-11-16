;; Regression test FERMI-DIRAC for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST FERMI-DIRAC
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6224593312018546d0 4.040305821324309d-16)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-M1 0.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.1368710061148999d0 1.6653345369377348d-16)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-0 0.75d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.42589430612383183d0 5.594591187346375d-16)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-1 -0.75d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.1301630162645293d0 6.04246936785309d-16)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-2 0.25d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6668.827087650077d0 2.1696654306572242d-10)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-INTEGRAL 5 12.35d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.4642945890876293d0 4.053118505064653d-15)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-M1/2 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.8237212774015843d0 2.909989668869937d-15)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-1/2 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 4.165414459868321d0 3.666976569654393d-15)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-3/2 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.7014132779827524d0 8.881784197001252d-16)
                        (MULTIPLE-VALUE-LIST (FERMI-DIRAC-INC-0 2.0d0 0.5d0))))

