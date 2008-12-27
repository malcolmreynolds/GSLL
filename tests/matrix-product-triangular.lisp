;; Regression test MATRIX-PRODUCT-TRIANGULAR for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-PRODUCT-TRIANGULAR
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34803.82 7799.5503 -29131.375)
                             (-4884.0327 -739.594 20382.809)
                             (-31391.57 -25366.535 -31634.615)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5))))
                               (M2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((42.73 -17.24 43.31)
                                               (-16.12 -8.25 21.44)
                                               (-49.08 -39.66 -49.46))))
                               (S1 19.68))
                           (CL-ARRAY (MATRIX-PRODUCT-TRIANGULAR M1 M2 S1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34803.82416d0 7799.550047999999d0
                              -29131.375104000002d0)
                             (-4884.032832000001d0 -739.5940799999992d0
                              20382.808224d0)
                             (-31391.568d0 -25366.535999999996d0
                              -31634.616d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (M2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((42.73d0 -17.24d0 43.31d0)
                                               (-16.12d0 -8.25d0 21.44d0)
                                               (-49.08d0 -39.66d0 -49.46d0))))
                               (S1 19.68d0))
                           (CL-ARRAY (MATRIX-PRODUCT-TRIANGULAR M1 M2 S1)))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LET ((M1
                                                      (MAKE-MARRAY
                                                       '(COMPLEX SINGLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((-34.5 8.24 3.29 -8.93
                                                          34.12 -6.15)
                                                         (-8.93 34.12 -6.15
                                                          49.27 -13.49 32.5)
                                                         (49.27 -13.49 32.5
                                                          42.73 -17.24
                                                          43.31))))
                                                     (M2
                                                      (MAKE-MARRAY
                                                       '(COMPLEX SINGLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((42.73 -17.24 43.31
                                                          -16.12 -8.25 21.44)
                                                         (-16.12 -8.25 21.44
                                                          -49.08 -39.66 -49.46)
                                                         (-49.08 -39.66 -49.46
                                                          19.68 -5.55 -8.82))))
                                                     (S1 #C(19.68 -5.55)))
                                                 (CL-ARRAY
                                                  (MATRIX-PRODUCT-TRIANGULAR M1
                                                                             M2
                                                                             S1))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LET ((M1
                                                      (MAKE-MARRAY
                                                       '(COMPLEX DOUBLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((-34.5d0 8.24d0 3.29d0
                                                          -8.93d0 34.12d0
                                                          -6.15d0)
                                                         (-8.93d0 34.12d0
                                                          -6.15d0 49.27d0
                                                          -13.49d0 32.5d0)
                                                         (49.27d0 -13.49d0
                                                          32.5d0 42.73d0
                                                          -17.24d0 43.31d0))))
                                                     (M2
                                                      (MAKE-MARRAY
                                                       '(COMPLEX DOUBLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((42.73d0 -17.24d0
                                                          43.31d0 -16.12d0
                                                          -8.25d0 21.44d0)
                                                         (-16.12d0 -8.25d0
                                                          21.44d0 -49.08d0
                                                          -39.66d0 -49.46d0)
                                                         (-49.08d0 -39.66d0
                                                          -49.46d0 19.68d0
                                                          -5.55d0 -8.82d0))))
                                                     (S1 #C(19.68d0 -5.55d0)))
                                                 (CL-ARRAY
                                                  (MATRIX-PRODUCT-TRIANGULAR M1
                                                                             M2
                                                                             S1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-1473.7527 -854.58527 1407.5751))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5))))
                               (V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(42.73 -17.24 43.31)))
                               (S1 -16.12))
                           (CL-ARRAY (MATRIX-PRODUCT-TRIANGULAR M1 V1 S1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-1473.7527d0 -854.5853d0 1407.575d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(42.73d0 -17.24d0 43.31d0)))
                               (S1 -16.12d0))
                           (CL-ARRAY (MATRIX-PRODUCT-TRIANGULAR M1 V1 S1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-1483.223 1289.3523) #C(-57.63159 1675.6711)
                           #C(-786.3365 -726.9331)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29 -8.93 34.12
                                                -6.15)
                                               (-8.93 34.12 -6.15 49.27 -13.49
                                                32.5)
                                               (49.27 -13.49 32.5 42.73 -17.24
                                                43.31))))
                               (V1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(42.73 -17.24 43.31 -16.12 -8.25
                                               21.44)))
                               (S1 #C(-16.12 -8.25)))
                           (CL-ARRAY (MATRIX-PRODUCT-TRIANGULAR M1 V1 S1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-1483.2230999999997d0 1289.3523999999998d0)
                           #C(-57.63160000000005d0 1675.6711000000003d0)
                           #C(-786.3364000000001d0 -726.9331d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0 -8.93d0
                                                34.12d0 -6.15d0)
                                               (-8.93d0 34.12d0 -6.15d0 49.27d0
                                                -13.49d0 32.5d0)
                                               (49.27d0 -13.49d0 32.5d0 42.73d0
                                                -17.24d0 43.31d0))))
                               (V1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0)))
                               (S1 #C(-16.12d0 -8.25d0)))
                           (CL-ARRAY (MATRIX-PRODUCT-TRIANGULAR M1 V1 S1))))))

