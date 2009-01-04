;; Regression test MATRIX-SUB for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-SUB
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-77.229996 25.48 -40.02)
                             (7.1900005 42.37 -27.59)
                             (98.350006 26.17 81.96)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5))))
                               (M2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((42.73 -17.24 43.31)
                                               (-16.12 -8.25 21.44)
                                               (-49.08 -39.66 -49.46)))))
                           (CL-ARRAY (ELT- M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-77.22999999999999d0 25.479999999999997d0
                              -40.02d0)
                             (7.190000000000001d0 42.37d0
                              -27.590000000000003d0)
                             (98.35d0 26.169999999999995d0
                              81.96000000000001d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (M2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((42.73d0 -17.24d0 43.31d0)
                                               (-16.12d0 -8.25d0 21.44d0)
                                               (-49.08d0 -39.66d0 -49.46d0)))))
                           (CL-ARRAY (ELT- M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-96 -96 41) (-128 125 -2) (88 17 55)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123))))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
                                             '((32 28 30) (37 -73 -8)
                                               (-15 -22 68)))))
                           (CL-ARRAY (ELT- M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((39 34 170) (88 241 179) (125 106 132)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98))))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS
                                             '((28 10 19) (28 178 217)
                                               (36 109 222)))))
                           (CL-ARRAY (ELT- M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-96 -96 41) (-128 125 -2) (88 17 55)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123))))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((32 28 30) (37 -73 -8)
                                               (-15 -22 68)))))
                           (CL-ARRAY (ELT- M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((39 34 170) (88 65521 65459) (125 106 65412)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98))))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((28 10 19) (28 178 217)
                                               (36 109 222)))))
                           (CL-ARRAY (ELT- M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-96 -96 41) (-128 125 -2) (88 17 55)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123))))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((32 28 30) (37 -73 -8)
                                               (-15 -22 68)))))
                           (CL-ARRAY (ELT- M1 M2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((39 34 170)
                             (88 4294967281 4294967219)
                             (125 106 4294967172)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98))))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((28 10 19) (28 178 217)
                                               (36 109 222)))))
                           (CL-ARRAY (ELT- M1 M2)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-96 -96 41) (-128 125 -2) (88 17 55)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123))))
                               (M2
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((32 28 30) (37 -73 -8)
                                               (-15 -22 68)))))
                           (CL-ARRAY (ELT- M1 M2)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((39 34 170)
                             (88 18446744073709551601 18446744073709551539)
                             (125 106 18446744073709551492)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98))))
                               (M2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((28 10 19) (28 178 217)
                                               (36 109 222)))))
                           (CL-ARRAY (ELT- M1 M2))))))

