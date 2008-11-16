;; Regression test DOT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST DOT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 6.5285645)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SINGLE-FLOAT
                                                               (A -34.5 8.24
                                                                  3.29 -8.93
                                                                  34.12 -6.15
                                                                  49.27 -13.49)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SINGLE-FLOAT
                                                               (A 32.5 42.73
                                                                  -17.24 43.31
                                                                  -16.12 -8.25
                                                                  21.44 -49.08)
                                                               NIL)))
                                                            (DOT V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6.528400000000033d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0
                                49.27d0 -13.49d0)
                             NIL))
                           (V2
                            (VECTOR-DOUBLE-FLOAT
                             (A 32.5d0 42.73d0 -17.24d0 43.31d0 -16.12d0
                                -8.25d0 21.44d0 -49.08d0)
                             NIL)))
                          (DOT V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-8.778225633844878d26 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A -34.5 8.24 3.29 -8.93 34.12 -6.15 49.27 -13.49
                                32.5 42.73 -17.24 43.31 -16.12 -8.25 21.44
                                -49.08)
                             NIL))
                           (V2
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A 32.5 42.73 -17.24 43.31 -16.12 -8.25 21.44
                                -49.08 -39.66 -49.46 19.68 -5.55 -8.82 25.37
                                -30.58 31.67)
                             NIL)))
                          (DOT V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #C(625.9736000000007d0 -4310.1183d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0
                                49.27d0 -13.49d0 32.5d0 42.73d0 -17.24d0
                                43.31d0 -16.12d0 -8.25d0 21.44d0 -49.08d0)
                             NIL))
                           (V2
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A 32.5d0 42.73d0 -17.24d0 43.31d0 -16.12d0
                                -8.25d0 21.44d0 -49.08d0 -39.66d0 -49.46d0
                                19.68d0 -5.55d0 -8.82d0 25.37d0 -30.58d0
                                31.67d0)
                             NIL)))
                          (DOT V1 V2)))))

