;; Regression test MATRIX-PRODUCT-SYMMETRIC for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-PRODUCT-SYMMETRIC
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34803.824 7799.5503 -29131.375)
                             (2045.2012 -3535.288 27406.096)
                             (-26673.887 -25484.264 -31425.34)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL))
                           (M2
                            (MATRIX-SINGLE-FLOAT
                             (A (42.73 -17.24 43.31) (-16.12 -8.25 21.44)
                                (-49.08 -39.66 -49.46))
                             NIL))
                           (ANSWER (MATRIX-SINGLE-FLOAT '(3 3) NIL)) (S1 19.68)
                           (S2 -5.55))
                          (CL-ARRAY
                           (MATRIX-PRODUCT-SYMMETRIC M1 M2 ANSWER S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34803.82416d0 7799.550048d0 -29131.375104d0)
                             (2045.2007039999999d0 -3535.2876479999995d0
                              27406.096416d0)
                             (-26673.884303999996d0 -25484.263728d0
                              -31425.340848d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL))
                           (M2
                            (MATRIX-DOUBLE-FLOAT
                             (A (42.73d0 -17.24d0 43.31d0)
                                (-16.12d0 -8.25d0 21.44d0)
                                (-49.08d0 -39.66d0 -49.46d0))
                             NIL))
                           (ANSWER (MATRIX-DOUBLE-FLOAT '(3 3) NIL))
                           (S1 19.68d0) (S2 -5.55d0))
                          (CL-ARRAY
                           (MATRIX-PRODUCT-SYMMETRIC M1 M2 ANSWER S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(23756.895 8100.1406 -26665.428))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL))
                           (V1
                            (VECTOR-SINGLE-FLOAT (A 42.73 -17.24 43.31) NIL))
                           (ANSWER (VECTOR-SINGLE-FLOAT '3 NIL)) (S1 -16.12)
                           (S2 -8.25))
                          (CL-ARRAY
                           (MATRIX-PRODUCT-SYMMETRIC M1 V1 ANSWER S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(23756.893524d0 8100.140412d0 -26665.425124000005d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL))
                           (V1
                            (VECTOR-DOUBLE-FLOAT (A 42.73d0 -17.24d0 43.31d0)
                                                 NIL))
                           (ANSWER (VECTOR-DOUBLE-FLOAT '3 NIL)) (S1 -16.12d0)
                           (S2 -8.25d0))
                          (CL-ARRAY
                           (MATRIX-PRODUCT-SYMMETRIC M1 V1 ANSWER S1 S2))))))

