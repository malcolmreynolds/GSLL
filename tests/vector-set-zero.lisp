;; Regression test VECTOR-SET-ZERO for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-SET-ZERO
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0.0 0.0 0.0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SINGLE-FLOAT
                                                               (A -34.5 8.24
                                                                  3.29)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(0.0d0 0.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL)))
                          (SET-ZERO V1) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(#C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A -34.5 8.24 3.29 -8.93 34.12 -6.15) NIL)))
                          (SET-ZERO V1) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0)
                             NIL)))
                          (SET-ZERO V1) (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (SET-ZERO V1)
                                                            (CL-ARRAY V1)))))

