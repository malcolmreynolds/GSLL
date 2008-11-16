;; Regression test MATRIX-M+ for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-M+
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((8.23 -9.0 46.600002)
                             (-25.050001 25.869999 15.290001)
                             (0.18999863 -53.15 -16.96)))
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
                             NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((8.229999999999997d0 -8.999999999999998d0 46.6d0)
                             (-25.05d0 25.869999999999997d0
                              15.290000000000001d0)
                             (0.19000000000000483d0 -53.15d0 -16.96d0)))
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
                             NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -40 101) (-54 -21 -18) (58 -27 -65)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-8
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2
                            (MATRIX-SIGNED-BYTE-8
                             (A (32 28 30) (37 -73 -8) (-15 -22 68)) NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 54 208) (144 85 101) (197 68 64)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (28 10 19) (28 178 217) (36 109 222)) NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -40 101) (-54 -21 -18) (58 -27 191)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-16
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2
                            (MATRIX-SIGNED-BYTE-16
                             (A (32 28 30) (37 -73 -8) (-15 -22 68)) NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 54 208) (144 341 357) (197 324 320)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (28 10 19) (28 178 217) (36 109 222)) NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -40 101) (-54 -21 -18) (58 -27 191)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-32
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2
                            (MATRIX-SIGNED-BYTE-32
                             (A (32 28 30) (37 -73 -8) (-15 -22 68)) NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 54 208) (144 341 357) (197 324 320)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (28 10 19) (28 178 217) (36 109 222)) NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -40 101) (-54 -21 -18) (58 -27 191)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-64
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2
                            (MATRIX-SIGNED-BYTE-64
                             (A (32 28 30) (37 -73 -8) (-15 -22 68)) NIL)))
                          (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 54 208) (144 341 357) (197 324 320)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (28 10 19) (28 178 217) (36 109 222)) NIL)))
                          (CL-ARRAY (M+ M1 M2))))))

