;; Regression test MATRIX-PRODUCT-NONSQUARE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-PRODUCT-NONSQUARE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-1488.7347 959.9901) (774.94495 1312.0566)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15))))
                               (M2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((49.27 -13.49) (32.5 42.73)
                                               (-17.24 43.31)))))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-1488.7346d0 959.9901d0)
                             (774.9448999999998d0 1312.0567999999996d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0))))
                               (M2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((49.27d0 -13.49d0)
                                               (32.5d0 42.73d0)
                                               (-17.24d0 43.31d0)))))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-1422.0259 2305.5098)
                              #C(-1744.0583 -1092.294))
                             (#C(-3459.892 1995.4918)
                              #C(-3290.4465 -801.0577))))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29 -8.93 34.12
                                                -6.15)
                                               (-8.93 34.12 -6.15 49.27 -13.49
                                                32.5))))
                               (M2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((49.27 -13.49 32.5 42.73)
                                               (32.5 42.73 -17.24 43.31)
                                               (-17.24 43.31 -16.12 -8.25)))))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-1422.0258d0 2305.5097000000005d0)
                              #C(-1744.0584d0 -1092.2939d0))
                             (#C(-3459.8918d0 1995.4917d0)
                              #C(-3290.4465d0 -801.0577000000002d0))))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0 -8.93d0
                                                34.12d0 -6.15d0)
                                               (-8.93d0 34.12d0 -6.15d0 49.27d0
                                                -13.49d0 32.5d0))))
                               (M2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((49.27d0 -13.49d0 32.5d0
                                                42.73d0)
                                               (32.5d0 42.73d0 -17.24d0
                                                43.31d0)
                                               (-17.24d0 43.31d0 -16.12d0
                                                -8.25d0)))))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2))))))

