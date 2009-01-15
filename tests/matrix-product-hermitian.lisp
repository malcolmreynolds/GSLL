;; Regression test MATRIX-PRODUCT-HERMITIAN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-PRODUCT-HERMITIAN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(161635.16 -134299.77) #C(93510.016 -191870.72)
                              #C(55739.336 13621.439))
                             (#C(-68526.41 125188.29) #C(46318.242 59787.605)
                              #C(-28142.984 5816.9385))
                             (#C(-139503.38 55690.902) #C(2898.8164 49116.78)
                              #C(-57882.3 -176350.63))))
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
                               (M2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((42.73 -17.24 43.31 -16.12 -8.25
                                                21.44)
                                               (-16.12 -8.25 21.44 -49.08
                                                -39.66 -49.46)
                                               (-49.08 -39.66 -49.46 19.68
                                                -5.55 -8.82))))
                               (M3
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((19.68 -5.55 -8.82 25.37 -30.58
                                                31.67)
                                               (25.37 -30.58 31.67 29.36 -33.24
                                                -27.03)
                                               (29.36 -33.24 -27.03 -41.67 42.0
                                                -20.81))))
                               (S1 #C(-41.67 42.0))
                               (S2 #C(42.0 -20.81)))
                           (CL-ARRAY
                            (MATRIX-PRODUCT-HERMITIAN M1 M2 M3 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(161635.17690299999d0 -134299.761873d0)
                              #C(93510.01525000001d0 -191870.723694d0)
                              #C(55739.340844000006d0 13621.441385000013d0))
                             (#C(-68526.41140699999d0 125188.28846599998d0)
                              #C(46318.25349699999d0 59787.61715899999d0)
                              #C(-28142.984856000003d0 5816.936996999999d0))
                             (#C(-139503.391422d0 55690.900627999996d0)
                              #C(2898.816300000017d0 49116.78897299998d0)
                              #C(-57882.29909799999d0 -176350.62442500002d0))))
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
                               (M2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((42.73d0 -17.24d0 43.31d0
                                                -16.12d0 -8.25d0 21.44d0)
                                               (-16.12d0 -8.25d0 21.44d0
                                                -49.08d0 -39.66d0 -49.46d0)
                                               (-49.08d0 -39.66d0 -49.46d0
                                                19.68d0 -5.55d0 -8.82d0))))
                               (M3
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((19.68d0 -5.55d0 -8.82d0 25.37d0
                                                -30.58d0 31.67d0)
                                               (25.37d0 -30.58d0 31.67d0
                                                29.36d0 -33.24d0 -27.03d0)
                                               (29.36d0 -33.24d0 -27.03d0
                                                -41.67d0 42.0d0 -20.81d0))))
                               (S1 #C(-41.67d0 42.0d0))
                               (S2 #C(42.0d0 -20.81d0)))
                           (CL-ARRAY
                            (MATRIX-PRODUCT-HERMITIAN M1 M2 M3 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(117171.67 19582.54) #C(18787.117 29534.742)
                           #C(-104992.03 72729.13)))
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
                               (V2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-16.12 -8.25 21.44 -49.08 -39.66
                                               -49.46)))
                               (S1 #C(-49.08 -39.66))
                               (S2 #C(-39.66 -49.46)))
                           (CL-ARRAY
                            (MATRIX-PRODUCT-HERMITIAN M1 V1 V2 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(117171.67150799997d0 19582.539385999997d0)
                           #C(18787.113150000005d0 29534.74247000001d0)
                           #C(-104992.03586199998d0 72729.12516600001d0)))
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
                               (V2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-16.12d0 -8.25d0 21.44d0
                                               -49.08d0 -39.66d0 -49.46d0)))
                               (S1 #C(-49.08d0 -39.66d0))
                               (S2 #C(-39.66d0 -49.46d0)))
                           (CL-ARRAY
                            (MATRIX-PRODUCT-HERMITIAN M1 V1 V2 S1 S2))))))

