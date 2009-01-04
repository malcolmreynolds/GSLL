;; Regression test MATRIX-SET-ALL-ADD for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-SET-ALL-ADD
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((8.23 50.97 46.02)
                             (33.8 76.85 36.579998)
                             (92.0 29.24 75.229996)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :DIMENSIONS '(3 3)))
                               (M2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5)))))
                           (SET-ALL M1 42.73)
                           (CL-ARRAY (ELT+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((8.229999999999997d0 50.97d0 46.019999999999996d0)
                             (33.8d0 76.85d0 36.58d0)
                             (92.0d0 29.239999999999995d0
                              75.22999999999999d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '(3 3)))
                               (M2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0)))))
                           (SET-ALL M1 42.73d0)
                           (CL-ARRAY (ELT+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -36 103) (-59 84 22) (105 27 -101)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :DIMENSIONS
                                             '(3 3)))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123)))))
                           (SET-ALL M1 32)
                           (CL-ARRAY (ELT+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 72 217) (144 191 168) (189 243 126)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8) :DIMENSIONS
                                             '(3 3)))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (SET-ALL M1 28)
                           (CL-ARRAY (ELT+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -36 103) (-59 84 22) (105 27 155)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 16) :DIMENSIONS
                                             '(3 3)))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123)))))
                           (SET-ALL M1 32)
                           (CL-ARRAY (ELT+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 72 217) (144 191 168) (189 243 126)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16) :DIMENSIONS
                                             '(3 3)))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (SET-ALL M1 28)
                           (CL-ARRAY (ELT+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -36 103) (-59 84 22) (105 27 155)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 32) :DIMENSIONS
                                             '(3 3)))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123)))))
                           (SET-ALL M1 32)
                           (CL-ARRAY (ELT+ M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 72 217) (144 191 168) (189 243 126)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32) :DIMENSIONS
                                             '(3 3)))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (SET-ALL M1 28)
                           (CL-ARRAY (ELT+ M1 M2)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-32 -36 103) (-59 84 22) (105 27 155)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 64) :DIMENSIONS
                                             '(3 3)))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123)))))
                           (SET-ALL M1 32)
                           (CL-ARRAY (ELT+ M1 M2)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((95 72 217) (144 191 168) (189 243 126)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64) :DIMENSIONS
                                             '(3 3)))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (SET-ALL M1 28)
                           (CL-ARRAY (ELT+ M1 M2))))))

