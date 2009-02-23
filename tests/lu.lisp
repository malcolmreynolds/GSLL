;; Regression test LU for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LU
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #( 42.730000000000004d0 -17.24d0 43.309999999999995d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((MAT
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (VEC
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(42.73d0 -17.24d0 43.31d0))))
                           (MULTIPLE-VALUE-BIND
                               (MATRIX PERM)
                               (LU-DECOMPOSITION MAT)
                             (LET ((X (LU-SOLVE MATRIX VEC PERM)))
                               (CL-ARRAY
                                (MATRIX-PRODUCT-TRIANGULAR MATRIX
                                                           (MATRIX-PRODUCT-TRIANGULAR
                                                            MATRIX X 1 :UPPER
                                                            :NOTRANS :NONUNIT)
                                                           1 :LOWER :NOTRANS
                                                           :UNIT)))))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(42.730000000000004d0 -17.240000000000006d0)
                           #C(43.310000000000024d0 -16.11999999999999d0)
                           #C(-8.249999999999986d0 21.440000000000005d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((MAT
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0 -8.93d0
                                                34.12d0 -6.15d0)
                                               (-8.93d0 34.12d0 -6.15d0 49.27d0
                                                -13.49d0 32.5d0)
                                               (49.27d0 -13.49d0 32.5d0 42.73d0
                                                -17.24d0 43.31d0))))
                               (VEC
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0))))
                           (MULTIPLE-VALUE-BIND
                               (MATRIX PERM)
                               (LU-DECOMPOSITION MAT)
                             (LET ((X (LU-SOLVE MATRIX VEC PERM)))
                               (CL-ARRAY
                                (MATRIX-PRODUCT-TRIANGULAR MATRIX
                                                           (MATRIX-PRODUCT-TRIANGULAR
                                                            MATRIX X 1 :UPPER
                                                            :NOTRANS :NONUNIT)
                                                           1 :LOWER :NOTRANS
                                                           :UNIT)))))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-1.9999999999999998d0 1.0d0)
                             (1.4999999999999998d0 -0.49999999999999994d0)))
                        (MULTIPLE-VALUE-LIST
                         (CL-ARRAY
                          (INVERT-MATRIX
                           (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '(2 2)
                                        :INITIAL-CONTENTS
                                        '(1.0d0 2.0d0 3.0d0 4.0d0)))))))

