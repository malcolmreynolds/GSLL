;; Regression test MATRIX-ADD-SCALAR for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-ADD-SCALAR
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-16.31 26.43 21.48)
                             (9.259999 52.309998 12.04)
                             (67.46 4.7000003 50.69)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-16.31d0 26.43d0 21.48d0)
                             (9.260000000000002d0 52.31d0 12.040000000000001d0)
                             (67.46000000000001d0 4.700000000000001d0
                              50.69d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-45 -49 89) (-72 70 8) (91 13 -115)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((85 62 207) (134 181 158) (179 233 116)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-45 -49 89) (-72 70 8) (91 13 141)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((85 62 207) (134 181 158) (179 233 116)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-45 -49 89) (-72 70 8) (91 13 141)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((85 62 207) (134 181 158) (179 233 116)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((-45 -49 89) (-72 70 8) (91 13 141)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((-64 -68 71) (-91 52 -10)
                                               (73 -5 123)))))
                           (CL-ARRAY (ELT+ M1 18.19d0)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #2A((85 62 207) (134 181 158) (179 233 116)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (CL-ARRAY (ELT+ M1 18.19d0))))))

