;; Regression test VECTOR-M+ for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-M+
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-43.43 42.36 -2.8600001))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SINGLE-FLOAT (A -34.5 8.24 3.29) NIL))
                           (V2
                            (VECTOR-SINGLE-FLOAT (A -8.93 34.12 -6.15) NIL)))
                          (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-43.43d0 42.36d0 -2.8600000000000003d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL))
                           (V2
                            (VECTOR-DOUBLE-FLOAT (A -8.93d0 34.12d0 -6.15d0)
                                                 NIL)))
                          (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(101 -16 61))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -64 -68 71)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -91 52 -10)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 207 73))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 67 44 189)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 116 163 140)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-155 -16 61))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A -64 -68 71)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A -91 52 -10)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 207 329))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 67 44 189)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 116 163 140)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-155 -16 61))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A -64 -68 71)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A -91 52 -10)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 207 329))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 67 44 189)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 116 163 140)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-155 -16 61))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A -64 -68 71)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A -91 52 -10)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 207 329))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 67 44 189)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 116 163 140)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+ V1 V2))))))

