;; Regression test MATRIX-PRODUCT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-PRODUCT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34803.824 7799.55 -29131.377)
                             (-12393.505 2290.2053 12771.404)
                             (14320.465 -39892.78 4668.254)))
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
                          (CL-ARRAY (MATRIX-PRODUCT M1 M2 ANSWER S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34803.824160000004d0 7799.550047999999d0
                              -29131.375104000002d0)
                             (-12393.505583999999d0 2290.2048959999997d0
                              12771.40488d0)
                             (14320.464911999996d0 -39892.782864d0
                              4668.255407999997d0)))
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
                          (CL-ARRAY (MATRIX-PRODUCT M1 M2 ANSWER S1 S2)))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((M1
                                                  (MATRIX-COMPLEX-SINGLE-FLOAT
                                                   (A
                                                    (-34.5 8.24 3.29 -8.93
                                                     34.12 -6.15)
                                                    (-8.93 34.12 -6.15 49.27
                                                     -13.49 32.5)
                                                    (49.27 -13.49 32.5 42.73
                                                     -17.24 43.31))
                                                   NIL))
                                                 (M2
                                                  (MATRIX-COMPLEX-SINGLE-FLOAT
                                                   (A
                                                    (42.73 -17.24 43.31 -16.12
                                                     -8.25 21.44)
                                                    (-16.12 -8.25 21.44 -49.08
                                                     -39.66 -49.46)
                                                    (-49.08 -39.66 -49.46 19.68
                                                     -5.55 -8.82))
                                                   NIL))
                                                 (ANSWER
                                                  (MATRIX-COMPLEX-SINGLE-FLOAT
                                                   '(3 3) NIL))
                                                 (S1 #C(19.68 -5.55))
                                                 (S2 #C(-5.55 -8.82)))
                                                (CL-ARRAY
                                                 (MATRIX-PRODUCT M1 M2 ANSWER
                                                                 S1 S2))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((M1
                                                  (MATRIX-COMPLEX-DOUBLE-FLOAT
                                                   (A
                                                    (-34.5d0 8.24d0 3.29d0
                                                     -8.93d0 34.12d0 -6.15d0)
                                                    (-8.93d0 34.12d0 -6.15d0
                                                     49.27d0 -13.49d0 32.5d0)
                                                    (49.27d0 -13.49d0 32.5d0
                                                     42.73d0 -17.24d0 43.31d0))
                                                   NIL))
                                                 (M2
                                                  (MATRIX-COMPLEX-DOUBLE-FLOAT
                                                   (A
                                                    (42.73d0 -17.24d0 43.31d0
                                                     -16.12d0 -8.25d0 21.44d0)
                                                    (-16.12d0 -8.25d0 21.44d0
                                                     -49.08d0 -39.66d0
                                                     -49.46d0)
                                                    (-49.08d0 -39.66d0 -49.46d0
                                                     19.68d0 -5.55d0 -8.82d0))
                                                   NIL))
                                                 (ANSWER
                                                  (MATRIX-COMPLEX-DOUBLE-FLOAT
                                                   '(3 3) NIL))
                                                 (S1 #C(19.68d0 -5.55d0))
                                                 (S2 #C(-5.55d0 -8.82d0)))
                                                (CL-ARRAY
                                                 (MATRIX-PRODUCT M1 M2 ANSWER
                                                                 S1 S2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(23756.895 19926.967 -60376.652))
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
                          (CL-ARRAY (MATRIX-PRODUCT M1 V1 ANSWER S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(23756.893524000003d0 19926.966904d0
                           -60376.649164d0))
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
                          (CL-ARRAY (MATRIX-PRODUCT M1 V1 ANSWER S1 S2)))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((M1
                                                  (MATRIX-COMPLEX-SINGLE-FLOAT
                                                   (A
                                                    (-34.5 8.24 3.29 -8.93
                                                     34.12 -6.15)
                                                    (-8.93 34.12 -6.15 49.27
                                                     -13.49 32.5)
                                                    (49.27 -13.49 32.5 42.73
                                                     -17.24 43.31))
                                                   NIL))
                                                 (V1
                                                  (VECTOR-COMPLEX-SINGLE-FLOAT
                                                   (A 42.73 -17.24 43.31 -16.12
                                                      -8.25 21.44)
                                                   NIL))
                                                 (ANSWER
                                                  (VECTOR-COMPLEX-SINGLE-FLOAT
                                                   '3 NIL))
                                                 (S1 #C(-16.12 -8.25))
                                                 (S2 #C(-8.25 21.44)))
                                                (CL-ARRAY
                                                 (MATRIX-PRODUCT M1 V1 ANSWER
                                                                 S1 S2))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((M1
                                                  (MATRIX-COMPLEX-DOUBLE-FLOAT
                                                   (A
                                                    (-34.5d0 8.24d0 3.29d0
                                                     -8.93d0 34.12d0 -6.15d0)
                                                    (-8.93d0 34.12d0 -6.15d0
                                                     49.27d0 -13.49d0 32.5d0)
                                                    (49.27d0 -13.49d0 32.5d0
                                                     42.73d0 -17.24d0 43.31d0))
                                                   NIL))
                                                 (V1
                                                  (VECTOR-COMPLEX-DOUBLE-FLOAT
                                                   (A 42.73d0 -17.24d0 43.31d0
                                                      -16.12d0 -8.25d0 21.44d0)
                                                   NIL))
                                                 (ANSWER
                                                  (VECTOR-COMPLEX-DOUBLE-FLOAT
                                                   '3 NIL))
                                                 (S1 #C(-16.12d0 -8.25d0))
                                                 (S2 #C(-8.25d0 21.44d0)))
                                                (CL-ARRAY
                                                 (MATRIX-PRODUCT M1 V1 ANSWER
                                                                 S1 S2)))))

