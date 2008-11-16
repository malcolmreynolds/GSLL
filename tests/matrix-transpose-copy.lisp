;; Regression test MATRIX-TRANSPOSE-COPY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-TRANSPOSE-COPY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34.5 -8.93 49.27)
                             (8.24 34.12 -13.49)
                             (3.29 -6.15 32.5)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL))
                           (M2 (MATRIX-SINGLE-FLOAT '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34.5d0 -8.93d0 49.27d0)
                             (8.24d0 34.12d0 -13.49d0)
                             (3.29d0 -6.15d0 32.5d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL))
                           (M2 (MATRIX-DOUBLE-FLOAT '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-34.5 8.24) #C(-8.93 34.12) #C(49.27 -13.49))
                             (#C(3.29 -8.93) #C(-6.15 49.27) #C(32.5 42.73))
                             (#C(34.12 -6.15) #C(-13.49 32.5)
                              #C(-17.24 43.31))))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-COMPLEX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29 -8.93 34.12 -6.15)
                                (-8.93 34.12 -6.15 49.27 -13.49 32.5)
                                (49.27 -13.49 32.5 42.73 -17.24 43.31))
                             NIL))
                           (M2 (MATRIX-COMPLEX-SINGLE-FLOAT '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-34.5d0 8.24d0) #C(-8.93d0 34.12d0)
                              #C(49.27d0 -13.49d0))
                             (#C(3.29d0 -8.93d0) #C(-6.15d0 49.27d0)
                              #C(32.5d0 42.73d0))
                             (#C(34.12d0 -6.15d0) #C(-13.49d0 32.5d0)
                              #C(-17.24d0 43.31d0))))
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
                           (M2 (MATRIX-COMPLEX-DOUBLE-FLOAT '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -91 73) (-68 52 -5) (71 -10 123)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-8
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2 (MATRIX-SIGNED-BYTE-8 '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 116 161) (44 163 215) (189 140 98)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2 (MATRIX-UNSIGNED-BYTE-8 '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -91 73) (-68 52 -5) (71 -10 123)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-16
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2 (MATRIX-SIGNED-BYTE-16 '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 116 161) (44 163 215) (189 140 98)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2 (MATRIX-UNSIGNED-BYTE-16 '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -91 73) (-68 52 -5) (71 -10 123)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-32
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2 (MATRIX-SIGNED-BYTE-32 '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 116 161) (44 163 215) (189 140 98)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2 (MATRIX-UNSIGNED-BYTE-32 '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -91 73) (-68 52 -5) (71 -10 123)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-64
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2 (MATRIX-SIGNED-BYTE-64 '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 116 161) (44 163 215) (189 140 98)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2 (MATRIX-UNSIGNED-BYTE-64 '(3 3) NIL)))
                          (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1))))))

