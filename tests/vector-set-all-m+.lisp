;; Regression test VECTOR-SET-ALL-M+ for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-SET-ALL-M+
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-43.43 -0.69000053 -5.6400003))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SINGLE-FLOAT '3 NIL))
                           (V2 (VECTOR-SINGLE-FLOAT (A -34.5 8.24 3.29) NIL)))
                          (SET-ALL V1 -8.93) (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-43.43d0 -0.6899999999999995d0 -5.64d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-DOUBLE-FLOAT '3 NIL))
                           (V2
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL)))
                          (SET-ALL V1 -8.93d0) (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(101 97 -20))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-8
                                                               '3 NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (SET-ALL V1 -91)
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 160 49))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               '3 NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (SET-ALL V1 116)
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-155 -159 -20))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-16 '3 NIL))
                           (V2 (VECTOR-SIGNED-BYTE-16 (A -64 -68 71) NIL)))
                          (SET-ALL V1 -91) (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 160 305))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               '3 NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (SET-ALL V1 116)
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-155 -159 -20))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-32 '3 NIL))
                           (V2 (VECTOR-SIGNED-BYTE-32 (A -64 -68 71) NIL)))
                          (SET-ALL V1 -91) (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 160 305))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               '3 NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (SET-ALL V1 116)
                                                            (CL-ARRAY
                                                             (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-155 -159 -20))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-64 '3 NIL))
                           (V2 (VECTOR-SIGNED-BYTE-64 (A -64 -68 71) NIL)))
                          (SET-ALL V1 -91) (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 160 305))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               '3 NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (SET-ALL V1 116)
                                                            (CL-ARRAY
                                                             (M+ V1 V2))))))

