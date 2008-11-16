;; Regression test SETF-COLUMN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SETF-COLUMN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34.5 8.24 42.73)
                             (-8.93 34.12 -17.24)
                             (49.27 -13.49 43.31)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL))
                           (COL
                            (VECTOR-SINGLE-FLOAT (A 42.73 -17.24 43.31) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34.5d0 8.24d0 42.73d0)
                             (-8.93d0 34.12d0 -17.24d0)
                             (49.27d0 -13.49d0 43.31d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL))
                           (COL
                            (VECTOR-DOUBLE-FLOAT (A 42.73d0 -17.24d0 43.31d0)
                                                 NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-34.5 8.24) #C(3.29 -8.93) #C(42.73 -17.24))
                             (#C(-8.93 34.12) #C(-6.15 49.27) #C(43.31 -16.12))
                             (#C(49.27 -13.49) #C(32.5 42.73)
                              #C(-8.25 21.44))))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-COMPLEX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29 -8.93 34.12 -6.15)
                                (-8.93 34.12 -6.15 49.27 -13.49 32.5)
                                (49.27 -13.49 32.5 42.73 -17.24 43.31))
                             NIL))
                           (COL
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A 42.73 -17.24 43.31 -16.12 -8.25 21.44) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0)
                              #C(42.73d0 -17.24d0))
                             (#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0)
                              #C(43.31d0 -16.12d0))
                             (#C(49.27d0 -13.49d0) #C(32.5d0 42.73d0)
                              #C(-8.25d0 21.44d0))))
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
                           (COL
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A 42.73d0 -17.24d0 43.31d0 -16.12d0 -8.25d0
                                21.44d0)
                             NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -68 32) (-91 52 28) (73 -5 30)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-8
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (COL (VECTOR-SIGNED-BYTE-8 (A 32 28 30) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 44 28) (116 163 10) (161 215 19)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (COL (VECTOR-UNSIGNED-BYTE-8 (A 28 10 19) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -68 32) (-91 52 28) (73 -5 30)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-16
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (COL (VECTOR-SIGNED-BYTE-16 (A 32 28 30) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 44 28) (116 163 10) (161 215 19)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (COL (VECTOR-UNSIGNED-BYTE-16 (A 28 10 19) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -68 32) (-91 52 28) (73 -5 30)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-32
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (COL (VECTOR-SIGNED-BYTE-32 (A 32 28 30) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 44 28) (116 163 10) (161 215 19)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (COL (VECTOR-UNSIGNED-BYTE-32 (A 28 10 19) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -68 32) (-91 52 28) (73 -5 30)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-64
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (COL (VECTOR-SIGNED-BYTE-64 (A 32 28 30) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 44 28) (116 163 10) (161 215 19)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (COL (VECTOR-UNSIGNED-BYTE-64 (A 28 10 19) NIL)))
                          (SETF (COLUMN M1 2) COL) (CL-ARRAY M1)))))

