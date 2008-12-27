;; Regression test MATRIX-TRANSPOSE-COPY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-TRANSPOSE-COPY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34.5 -8.93 49.27)
                             (8.24 34.12 -13.49)
                             (3.29 -6.15 32.5)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5))))
                               (M2
                                (MAKE-MARRAY 'SINGLE-FLOAT :DIMENSIONS '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34.5d0 -8.93d0 49.27d0)
                             (8.24d0 34.12d0 -13.49d0)
                             (3.29d0 -6.15d0 32.5d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (M2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(-34.5 8.24) #C(-8.93 34.12) #C(49.27 -13.49))
                             (#C(3.29 -8.93) #C(-6.15 49.27) #C(32.5 42.73))
                             (#C(34.12 -6.15) #C(-13.49 32.5)
                              #C(-17.24 43.31))))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29 -8.93 34.12
                                                -6.15)
                                               (-8.93 34.12 -6.15 49.27 -13.49
                                                32.5)
                                               (49.27 -13.49 32.5 42.73 -17.24
                                                43.31))))
                               (M2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :DIMENSIONS '(3 3))))
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
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0 -8.93d0
                                                34.12d0 -6.15d0)
                                               (-8.93d0 34.12d0 -6.15d0 49.27d0
                                                -13.49d0 32.5d0)
                                               (49.27d0 -13.49d0 32.5d0 42.73d0
                                                -17.24d0 43.31d0))))
                               (M2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :DIMENSIONS '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -91 73) (-68 52 -5) (71 -10 123)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123))))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :DIMENSIONS
                                             '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 116 161) (44 163 215) (189 140 98)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98))))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8) :DIMENSIONS
                                             '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -91 73) (-68 52 -5) (71 -10 123)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123))))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 16) :DIMENSIONS
                                             '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 116 161) (44 163 215) (189 140 98)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98))))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16) :DIMENSIONS
                                             '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -91 73) (-68 52 -5) (71 -10 123)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123))))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 32) :DIMENSIONS
                                             '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 116 161) (44 163 215) (189 140 98)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98))))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32) :DIMENSIONS
                                             '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-64 -91 73) (-68 52 -5) (71 -10 123)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123))))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 64) :DIMENSIONS
                                             '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((67 116 161) (44 163 215) (189 140 98)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98))))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64) :DIMENSIONS
                                             '(3 3))))
                           (CL-ARRAY (MATRIX-TRANSPOSE-COPY M2 M1))))))

