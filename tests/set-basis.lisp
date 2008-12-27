;; Regression test SET-BASIS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SET-BASIS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29 -8.93 34.12
                                               -6.15 49.27 -13.49))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.0d0 0.0d0 1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0 -8.93d0
                                               34.12d0 -6.15d0 49.27d0
                                               -13.49d0))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(0.0 0.0) #C(0.0 0.0) #C(1.0 0.0) #C(0.0 0.0)
                           #C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29 -8.93 34.12
                                               -6.15 49.27 -13.49 32.5 42.73
                                               -17.24 43.31 -16.12 -8.25 21.44
                                               -49.08))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(1.0d0 0.0d0)
                           #C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0)
                           #C(0.0d0 0.0d0) #C(0.0d0 0.0d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0 -8.93d0
                                               34.12d0 -6.15d0 49.27d0 -13.49d0
                                               32.5d0 42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0
                                               -49.08d0))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
                                             '(-64 -68 71 -91 52 -10 73 -5))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS
                                             '(67 44 189 116 163 140 161 215))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '(-64 -68 71 -91 52 -10 73 -5))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '(67 44 189 116 163 140 161 215))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '(-64 -68 71 -91 52 -10 73 -5))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '(67 44 189 116 163 140 161 215))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '(-64 -68 71 -91 52 -10 73 -5))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '(67 44 189 116 163 140 161 215))))
                           (SET-BASIS V1 2)
                           (CL-ARRAY V1)))))

