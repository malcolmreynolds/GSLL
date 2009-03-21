;; Regression test LU for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LU
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(-39.65999999999999d0 -49.46000000000001d0
                           19.679999999999993d0 -5.549999999999997d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((MATRIX
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0 -8.93d0)
                                               (34.12d0 -6.15d0 49.27d0
                                                -13.49d0)
                                               (32.5d0 42.73d0 -17.24d0
                                                43.31d0)
                                               (-16.12d0 -8.25d0 21.44d0
                                                -49.08d0))))
                               (VEC
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-39.66d0 -49.46d0 19.68d0
                                               -5.55d0))))
                           (MULTIPLE-VALUE-BIND
                               (MATRIX PERM)
                               (LU-DECOMPOSITION MATRIX)
                             (LET ((X (LU-SOLVE MATRIX VEC PERM)))
                               (CL-ARRAY
                                (PERMUTE-INVERSE PERM
                                                 (MATRIX-PRODUCT-TRIANGULAR
                                                  MATRIX
                                                  (MATRIX-PRODUCT-TRIANGULAR
                                                   MATRIX X 1 :UPPER :NOTRANS
                                                   :NONUNIT)
                                                  1 :LOWER :NOTRANS
                                                  :UNIT))))))))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-39.65999999999999d0 -49.46000000000001d0)
                           #C(19.679999999999996d0 -5.549999999999995d0)
                           #C(-8.820000000000006d0 25.370000000000005d0)
                           #C(-30.580000000000002d0 31.67d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((MATRIX
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0 -8.93d0
                                                34.12d0 -6.15d0 49.27d0
                                                -13.49d0)
                                               (34.12d0 -6.15d0 49.27d0
                                                -13.49d0 32.5d0 42.73d0
                                                -17.24d0 43.31d0)
                                               (32.5d0 42.73d0 -17.24d0 43.31d0
                                                -16.12d0 -8.25d0 21.44d0
                                                -49.08d0)
                                               (-16.12d0 -8.25d0 21.44d0
                                                -49.08d0 -39.66d0 -49.46d0
                                                19.68d0 -5.55d0))))
                               (VEC
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-39.66d0 -49.46d0 19.68d0
                                               -5.55d0 -8.82d0 25.37d0 -30.58d0
                                               31.67d0))))
                           (MULTIPLE-VALUE-BIND
                               (MATRIX PERM)
                               (LU-DECOMPOSITION MATRIX)
                             (LET ((X (LU-SOLVE MATRIX VEC PERM)))
                               (CL-ARRAY
                                (PERMUTE-INVERSE PERM
                                                 (MATRIX-PRODUCT-TRIANGULAR
                                                  MATRIX
                                                  (MATRIX-PRODUCT-TRIANGULAR
                                                   MATRIX X 1 :UPPER :NOTRANS
                                                   :NONUNIT)
                                                  1 :LOWER :NOTRANS
                                                  :UNIT))))))))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-1.9999999999999998d0 1.0d0)
                             (1.4999999999999998d0 -0.49999999999999994d0)))
                        (MULTIPLE-VALUE-LIST
                         (CL-ARRAY
                          (INVERT-MATRIX
                           (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '(2 2)
                                        :INITIAL-CONTENTS
                                        '(1.0d0 2.0d0 3.0d0 4.0d0)))))))

