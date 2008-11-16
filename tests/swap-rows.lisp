;; Regression test SWAP-ROWS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SWAP-ROWS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-8.93 34.12 -6.15)
                             (-34.5 8.24 3.29)
                             (49.27 -13.49 32.5)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-8.93d0 34.12d0 -6.15d0)
                             (-34.5d0 8.24d0 3.29d0)
                             (49.27d0 -13.49d0 32.5d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-8.93 34.12) #C(-6.15 49.27) #C(-13.49 32.5))
                             (#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15))
                             (#C(49.27 -13.49) #C(32.5 42.73)
                              #C(-17.24 43.31))))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-COMPLEX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29 -8.93 34.12 -6.15)
                                (-8.93 34.12 -6.15 49.27 -13.49 32.5)
                                (49.27 -13.49 32.5 42.73 -17.24 43.31))
                             NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0)
                              #C(-13.49d0 32.5d0))
                             (#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0)
                              #C(34.12d0 -6.15d0))
                             (#C(49.27d0 -13.49d0) #C(32.5d0 42.73d0)
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
                             NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-91 52 -10) (-64 -68 71) (73 -5 123)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-8
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((116 163 140) (67 44 189) (161 215 98)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-91 52 -10) (-64 -68 71) (73 -5 123)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-16
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((116 163 140) (67 44 189) (161 215 98)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-91 52 -10) (-64 -68 71) (73 -5 123)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-32
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((116 163 140) (67 44 189) (161 215 98)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-91 52 -10) (-64 -68 71) (73 -5 123)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-64
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((116 163 140) (67 44 189) (161 215 98)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (CL-ARRAY (SWAP-ROWS M1 0 1))))))

