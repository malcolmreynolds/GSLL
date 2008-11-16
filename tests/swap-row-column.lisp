;; Regression test SWAP-ROW-COLUMN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SWAP-ROW-COLUMN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((3.29 -6.15 32.5)
                             (-8.93 34.12 8.24)
                             (49.27 -13.49 -34.5)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((3.29d0 -6.15d0 32.5d0)
                             (-8.93d0 34.12d0 8.24d0)
                             (49.27d0 -13.49d0 -34.5d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(34.12 -6.15) #C(-13.49 32.5) #C(-17.24 43.31))
                             (#C(-8.93 34.12) #C(-6.15 49.27) #C(3.29 -8.93))
                             (#C(49.27 -13.49) #C(32.5 42.73) #C(-34.5 8.24))))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-COMPLEX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29 -8.93 34.12 -6.15)
                                (-8.93 34.12 -6.15 49.27 -13.49 32.5)
                                (49.27 -13.49 32.5 42.73 -17.24 43.31))
                             NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(34.12d0 -6.15d0) #C(-13.49d0 32.5d0)
                              #C(-17.24d0 43.31d0))
                             (#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0)
                              #C(3.29d0 -8.93d0))
                             (#C(49.27d0 -13.49d0) #C(32.5d0 42.73d0)
                              #C(-34.5d0 8.24d0))))
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
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((71 -10 123) (-91 52 -68) (73 -5 -64)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-8
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((189 140 98) (116 163 44) (161 215 67)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((71 -10 123) (-91 52 -68) (73 -5 -64)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-16
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((189 140 98) (116 163 44) (161 215 67)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((71 -10 123) (-91 52 -68) (73 -5 -64)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-32
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((189 140 98) (116 163 44) (161 215 67)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((71 -10 123) (-91 52 -68) (73 -5 -64)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-64
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((189 140 98) (116 163 44) (161 215 67)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (CL-ARRAY (SWAP-ROW-COLUMN M1 0 2))))))

