;; Regression test SORT-MATRIX-LARGEST for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SORT-MATRIX-LARGEST
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((49.27 34.12 32.5) (8.24 3.29 -6.15)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL))
                           (M2 (MATRIX-SINGLE-FLOAT '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((49.27d0 34.12d0 32.5d0) (8.24d0 3.29d0 -6.15d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL))
                           (M2 (MATRIX-DOUBLE-FLOAT '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((123 73 71) (52 -5 -10)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-8
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2 (MATRIX-SIGNED-BYTE-8 '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((215 189 163) (161 140 116)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2 (MATRIX-UNSIGNED-BYTE-8 '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((123 73 71) (52 -5 -10)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-16
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2 (MATRIX-SIGNED-BYTE-16 '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((215 189 163) (161 140 116)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2 (MATRIX-UNSIGNED-BYTE-16 '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((123 73 71) (52 -5 -10)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-32
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2 (MATRIX-SIGNED-BYTE-32 '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((215 189 163) (161 140 116)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2 (MATRIX-UNSIGNED-BYTE-32 '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((123 73 71) (52 -5 -10)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-64
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL))
                           (M2 (MATRIX-SIGNED-BYTE-64 '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((215 189 163) (161 140 116)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL))
                           (M2 (MATRIX-UNSIGNED-BYTE-64 '(2 3) NIL)))
                          (CL-ARRAY (SORT-LARGEST M2 M1))))))

