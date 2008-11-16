;; Regression test COLUMN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST COLUMN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(8.24 34.12 -13.49))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL))
                           (COL (VECTOR-SINGLE-FLOAT '3 NIL)))
                          (CL-ARRAY (COLUMN COL M1 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(8.24d0 34.12d0 -13.49d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL))
                           (COL (VECTOR-DOUBLE-FLOAT '3 NIL)))
                          (CL-ARRAY (COLUMN COL M1 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(#C(3.29 -8.93) #C(-6.15 49.27) #C(32.5 42.73)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-COMPLEX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29 -8.93 34.12 -6.15)
                                (-8.93 34.12 -6.15 49.27 -13.49 32.5)
                                (49.27 -13.49 32.5 42.73 -17.24 43.31))
                             NIL))
                           (COL (VECTOR-COMPLEX-SINGLE-FLOAT '3 NIL)))
                          (CL-ARRAY (COLUMN COL M1 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(3.29d0 -8.93d0) #C(-6.15d0 49.27d0)
                           #C(32.5d0 42.73d0)))
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
                           (COL (VECTOR-COMPLEX-DOUBLE-FLOAT '3 NIL)))
                          (CL-ARRAY (COLUMN COL M1 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-68 52 -5))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((M1
                                                              (MATRIX-SIGNED-BYTE-8
                                                               (A (-64 -68 71)
                                                                  (-91 52 -10)
                                                                  (73 -5 123))
                                                               NIL))
                                                             (COL
                                                              (VECTOR-SIGNED-BYTE-8
                                                               '3 NIL)))
                                                            (CL-ARRAY
                                                             (COLUMN COL M1
                                                                     1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 163 215))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((M1
                                                              (MATRIX-UNSIGNED-BYTE-8
                                                               (A (67 44 189)
                                                                  (116 163 140)
                                                                  (161 215 98))
                                                               NIL))
                                                             (COL
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               '3 NIL)))
                                                            (CL-ARRAY
                                                             (COLUMN COL M1
                                                                     1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-68 52 -5))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((M1
                                                              (MATRIX-SIGNED-BYTE-16
                                                               (A (-64 -68 71)
                                                                  (-91 52 -10)
                                                                  (73 -5 123))
                                                               NIL))
                                                             (COL
                                                              (VECTOR-SIGNED-BYTE-16
                                                               '3 NIL)))
                                                            (CL-ARRAY
                                                             (COLUMN COL M1
                                                                     1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 163 215))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((M1
                                                              (MATRIX-UNSIGNED-BYTE-16
                                                               (A (67 44 189)
                                                                  (116 163 140)
                                                                  (161 215 98))
                                                               NIL))
                                                             (COL
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               '3 NIL)))
                                                            (CL-ARRAY
                                                             (COLUMN COL M1
                                                                     1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-68 52 -5))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((M1
                                                              (MATRIX-SIGNED-BYTE-32
                                                               (A (-64 -68 71)
                                                                  (-91 52 -10)
                                                                  (73 -5 123))
                                                               NIL))
                                                             (COL
                                                              (VECTOR-SIGNED-BYTE-32
                                                               '3 NIL)))
                                                            (CL-ARRAY
                                                             (COLUMN COL M1
                                                                     1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 163 215))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((M1
                                                              (MATRIX-UNSIGNED-BYTE-32
                                                               (A (67 44 189)
                                                                  (116 163 140)
                                                                  (161 215 98))
                                                               NIL))
                                                             (COL
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               '3 NIL)))
                                                            (CL-ARRAY
                                                             (COLUMN COL M1
                                                                     1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-68 52 -5))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((M1
                                                              (MATRIX-SIGNED-BYTE-64
                                                               (A (-64 -68 71)
                                                                  (-91 52 -10)
                                                                  (73 -5 123))
                                                               NIL))
                                                             (COL
                                                              (VECTOR-SIGNED-BYTE-64
                                                               '3 NIL)))
                                                            (CL-ARRAY
                                                             (COLUMN COL M1
                                                                     1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 163 215))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((M1
                                                              (MATRIX-UNSIGNED-BYTE-64
                                                               (A (67 44 189)
                                                                  (116 163 140)
                                                                  (161 215 98))
                                                               NIL))
                                                             (COL
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               '3 NIL)))
                                                            (CL-ARRAY
                                                             (COLUMN COL M1
                                                                     1))))))

