;; Regression test ROOTS-ONE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ROOTS-ONE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.2357177734375d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-ONE-EXAMPLE-NO-DERIVATIVE *BISECTION-FSOLVER*
                                                          NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.23606797749979d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-ONE-EXAMPLE-NO-DERIVATIVE
                          *FALSE-POSITION-FSOLVER* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.2360634081902244d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-ONE-EXAMPLE-NO-DERIVATIVE *BRENT-FSOLVER*
                                                          NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.236067977499978d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-ONE-EXAMPLE-DERIVATIVE *NEWTON-FDFSOLVER*
                                                       NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.2360679849648637d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-ONE-EXAMPLE-DERIVATIVE *SECANT-FDFSOLVER*
                                                       NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.23606797749979d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-ONE-EXAMPLE-DERIVATIVE *STEFFENSON-FDFSOLVER*
                                                       NIL))))

