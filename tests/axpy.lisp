;; Regression test AXPY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST AXPY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(1400.77 -284.0684 -147.7214 397.47382 -1369.3191
                           235.659 -1932.6083 485.93335))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29 -8.93 34.12
                                               -6.15 49.27 -13.49)))
                               (V2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(32.5 42.73 -17.24 43.31 -16.12
                                               -8.25 21.44 -49.08)))
                               (SCALAR -39.66))
                           (CL-ARRAY (AXPY SCALAR V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(1400.77d0 -284.06839999999994d0 -147.7214d0
                           397.4738d0 -1369.3191999999997d0 235.659d0
                           -1932.6082d0 485.93339999999995d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0 -8.93d0
                                               34.12d0 -6.15d0 49.27d0
                                               -13.49d0)))
                               (V2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(32.5d0 42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0
                                               -49.08d0)))
                               (SCALAR -39.66d0))
                           (CL-ARRAY (AXPY SCALAR V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(1808.3204 1422.3015) #C(-589.3992 234.75043)
                           #C(-1673.498 -1451.916) #C(-2599.8237 -1950.9608)
                           #C(784.81586 -3351.5815) #C(2845.531 -870.5343)
                           #C(222.45422 1149.8602) #C(-3308.3872 917.76044)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29 -8.93 34.12
                                               -6.15 49.27 -13.49 32.5 42.73
                                               -17.24 43.31 -16.12 -8.25 21.44
                                               -49.08)))
                               (V2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(32.5 42.73 -17.24 43.31 -16.12
                                               -8.25 21.44 -49.08 -39.66 -49.46
                                               19.68 -5.55 -8.82 25.37 -30.58
                                               31.67)))
                               (SCALAR #C(-39.66 -49.46)))
                           (CL-ARRAY (AXPY SCALAR V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(1808.3204d0 1422.3016000000002d0)
                           #C(-589.3992d0 234.75039999999998d0)
                           #C(-1673.4981999999998d0 -1451.9162000000001d0)
                           #C(-2599.8236d0 -1950.9608000000003d0)
                           #C(784.8158000000002d0 -3351.5818d0)
                           #C(2845.5309999999995d0 -870.5342d0)
                           #C(222.45420000000001d0 1149.8601999999998d0)
                           #C(-3308.3871999999997d0 917.7603999999995d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0 -8.93d0
                                               34.12d0 -6.15d0 49.27d0 -13.49d0
                                               32.5d0 42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0
                                               -49.08d0)))
                               (V2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(32.5d0 42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0
                                               -49.08d0 -39.66d0 -49.46d0
                                               19.68d0 -5.55d0 -8.82d0 25.37d0
                                               -30.58d0 31.67d0)))
                               (SCALAR #C(-39.66d0 -49.46d0)))
                           (CL-ARRAY (AXPY SCALAR V1 V2))))))

