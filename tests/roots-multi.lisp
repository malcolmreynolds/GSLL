;; Regression test ROOTS-MULTI for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ROOTS-MULTI
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9999999999999992d0 0.9999999999999716d0
                              7.771561172376096d-16 -2.686739719592879d-13)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-MULTI-EXAMPLE-NO-DERIVATIVE *HYBRID-UNSCALED*
                                                            NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 1.0d0 0.0d0 0.0d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-MULTI-EXAMPLE-NO-DERIVATIVE *HYBRID-SCALED*
                                                            NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 1.0000000000000004d0 0.0d0
                              4.440892098500626d-15)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-MULTI-EXAMPLE-NO-DERIVATIVE *DISCRETE-NEWTON*
                                                            NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 1.0000000000000568d0 0.0d0
                              5.684341886080801d-13)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-MULTI-EXAMPLE-NO-DERIVATIVE *BROYDEN* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 1.0d0 0.0d0 0.0d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-MULTI-EXAMPLE-DERIVATIVE *NEWTON-MFDFSOLVER*
                                                         NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.999999999999998d0 0.9999999999999964d0
                              1.9984014443252818d-15 4.440892098500626d-15)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-MULTI-EXAMPLE-DERIVATIVE *GNEWTON-MFDFSOLVER*
                                                         NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 1.0d0 0.0d0 0.0d0)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-MULTI-EXAMPLE-DERIVATIVE *POWELLS-HYBRID*
                                                         NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9999999999999996d0 1.0000000000000568d0
                              4.440892098500626d-16 5.773159728050814d-13)
                        (MULTIPLE-VALUE-LIST
                         (ROOTS-MULTI-EXAMPLE-DERIVATIVE
                          *POWELLS-HYBRID-UNSCALED* NIL))))

