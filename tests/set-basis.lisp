;; Regression test SET-BASIS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SET-BASIS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-SINGLE-FLOAT
                             (A -34.5 8.24 3.29 -8.93 34.12 -6.15 49.27 -13.49)
                             NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.0d0 0.0d0 1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0
                                49.27d0 -13.49d0)
                             NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(0.0 0.0) #C(0.0 0.0) #C(1.0 0.0) #C(0.0 0.0)
                           #C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A -34.5 8.24 3.29 -8.93 34.12 -6.15 49.27 -13.49
                                32.5 42.73 -17.24 43.31 -16.12 -8.25 21.44
                                -49.08)
                             NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(1.0d0 0.0d0)
                           #C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0)
                           #C(0.0d0 0.0d0) #C(0.0d0 0.0d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0
                                49.27d0 -13.49d0 32.5d0 42.73d0 -17.24d0
                                43.31d0 -16.12d0 -8.25d0 21.44d0 -49.08d0)
                             NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-SIGNED-BYTE-8
                             (A -64 -68 71 -91 52 -10 73 -5) NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-UNSIGNED-BYTE-8
                             (A 67 44 189 116 163 140 161 215) NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-SIGNED-BYTE-16
                             (A -64 -68 71 -91 52 -10 73 -5) NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-UNSIGNED-BYTE-16
                             (A 67 44 189 116 163 140 161 215) NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-SIGNED-BYTE-32
                             (A -64 -68 71 -91 52 -10 73 -5) NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-UNSIGNED-BYTE-32
                             (A 67 44 189 116 163 140 161 215) NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-SIGNED-BYTE-64
                             (A -64 -68 71 -91 52 -10 73 -5) NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0 0 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-UNSIGNED-BYTE-64
                             (A 67 44 189 116 163 140 161 215) NIL)))
                          (SET-BASIS V1 2) (CL-ARRAY V1)))))

