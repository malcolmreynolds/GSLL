;; Regression test INVERSE-MATRIX-PRODUCT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST INVERSE-MATRIX-PRODUCT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-30.708971 5.3737082 -25.897392)
                             (-14.654706 -9.08723 6.96797)
                             (-29.71983 -24.015656 -29.949932)))
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
                           (S1 19.68))
                          (CL-ARRAY (INVERSE-MATRIX-PRODUCT M1 M2 S1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-30.708968222785604d0 5.373707833953741d0
                              -25.89739157520614d0)
                             (-14.654705167282895d0 -9.087229795292632d0
                              6.967969411128146d0)
                             (-29.71982769230769d0 -24.015655384615382d0
                              -29.949932307692308d0)))
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
                           (S1 19.68d0))
                          (CL-ARRAY (INVERSE-MATRIX-PRODUCT M1 M2 S1)))))
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
                                                 (S1 #C(19.68 -5.55)))
                                                (CL-ARRAY
                                                 (INVERSE-MATRIX-PRODUCT M1 M2
                                                                         S1))))
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
                                                 (S1 #C(19.68d0 -5.55d0)))
                                                (CL-ARRAY
                                                 (INVERSE-MATRIX-PRODUCT M1 M2
                                                                         S1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-1.1747805 -0.26507667 1.3326154))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL))
                           (V1
                            (VECTOR-SINGLE-FLOAT (A 42.73 -17.24 43.31) NIL))
                           (S1 -16.12))
                          (CL-ARRAY (INVERSE-MATRIX-PRODUCT M1 V1 S1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(-1.1747804927980594d0 -0.26507665253855167d0
                           1.3326153846153848d0))
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
                           (S1 -16.12d0))
                          (CL-ARRAY (INVERSE-MATRIX-PRODUCT M1 V1 S1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-1.1218005 0.24990065) #C(-0.7679142 -0.9143634)
                           #C(0.4927793 -0.0056687724)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-COMPLEX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29 -8.93 34.12 -6.15)
                                (-8.93 34.12 -6.15 49.27 -13.49 32.5)
                                (49.27 -13.49 32.5 42.73 -17.24 43.31))
                             NIL))
                           (V1
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A 42.73 -17.24 43.31 -16.12 -8.25 21.44) NIL))
                           (S1 #C(-16.12 -8.25)))
                          (CL-ARRAY (INVERSE-MATRIX-PRODUCT M1 V1 S1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-1.1218005239477833d0 0.2499006037347623d0)
                           #C(-0.7679142088967384d0 -0.9143634017568469d0)
                           #C(0.49277927293827817d0 -0.005668775466541512d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-COMPLEX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0)
                                (-8.93d0 34.12d0 -6.15d0 49.27d0 -13.49d0
                                 32.5d0)
                                (49.27d0 -13.49d0 32.5d0 42.73d0 -17.24d0
                                 43.31d0))
                             NIL))
                           (V1
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A 42.73d0 -17.24d0 43.31d0 -16.12d0 -8.25d0
                                21.44d0)
                             NIL))
                           (S1 #C(-16.12d0 -8.25d0)))
                          (CL-ARRAY (INVERSE-MATRIX-PRODUCT M1 V1 S1))))))
