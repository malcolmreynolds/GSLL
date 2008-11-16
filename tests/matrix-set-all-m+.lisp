;; Regression test MATRIX-SET-ALL-M+ for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-SET-ALL-M+
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((8.23 50.97 46.02)
                             (33.8 76.85 36.579998)
                             (92.0 29.24 75.229996)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-SINGLE-FLOAT '(3 3) NIL))
                           (M2
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL)))
                          (SET-ALL M1 42.73) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((8.229999999999997d0 50.97d0 46.019999999999996d0)
                             (33.8d0 76.85d0 36.58d0)
                             (92.0d0 29.239999999999995d0
                              75.22999999999999d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-DOUBLE-FLOAT '(3 3) NIL))
                           (M2
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL)))
                          (SET-ALL M1 42.73d0) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -36 103) (-59 84 22) (105 27 -101)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-SIGNED-BYTE-8 '(3 3) NIL))
                           (M2
                            (MATRIX-SIGNED-BYTE-8
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (SET-ALL M1 32) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 72 217) (144 191 168) (189 243 126)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-UNSIGNED-BYTE-8 '(3 3) NIL))
                           (M2
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (SET-ALL M1 28) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -36 103) (-59 84 22) (105 27 155)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-SIGNED-BYTE-16 '(3 3) NIL))
                           (M2
                            (MATRIX-SIGNED-BYTE-16
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (SET-ALL M1 32) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 72 217) (144 191 168) (189 243 126)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-UNSIGNED-BYTE-16 '(3 3) NIL))
                           (M2
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (SET-ALL M1 28) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -36 103) (-59 84 22) (105 27 155)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-SIGNED-BYTE-32 '(3 3) NIL))
                           (M2
                            (MATRIX-SIGNED-BYTE-32
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (SET-ALL M1 32) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 72 217) (144 191 168) (189 243 126)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-UNSIGNED-BYTE-32 '(3 3) NIL))
                           (M2
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (SET-ALL M1 28) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -36 103) (-59 84 22) (105 27 155)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-SIGNED-BYTE-64 '(3 3) NIL))
                           (M2
                            (MATRIX-SIGNED-BYTE-64
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (SET-ALL M1 32) (CL-ARRAY (M+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 72 217) (144 191 168) (189 243 126)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1 (MATRIX-UNSIGNED-BYTE-64 '(3 3) NIL))
                           (M2
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (SET-ALL M1 28) (CL-ARRAY (M+ M1 M2))))))

