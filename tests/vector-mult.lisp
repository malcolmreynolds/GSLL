;; Regression test VECTOR-MULT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-MULT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(308.08502 281.14877 -20.2335))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SINGLE-FLOAT (A -34.5 8.24 3.29) NIL))
                           (V2
                            (VECTOR-SINGLE-FLOAT (A -8.93 34.12 -6.15) NIL)))
                          (CL-ARRAY (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(308.085d0 281.1488d0 -20.233500000000003d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL))
                           (V2
                            (VECTOR-DOUBLE-FLOAT (A -8.93d0 34.12d0 -6.15d0)
                                                 NIL)))
                          (CL-ARRAY (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-64 48 58))
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
                                                             (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(92 4 92))
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
                                                             (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(5824 -3536 -710))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-16 (A -64 -68 71) NIL))
                           (V2 (VECTOR-SIGNED-BYTE-16 (A -91 52 -10) NIL)))
                          (CL-ARRAY (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(7772 7172 26460))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-UNSIGNED-BYTE-16 (A 67 44 189) NIL))
                           (V2 (VECTOR-UNSIGNED-BYTE-16 (A 116 163 140) NIL)))
                          (CL-ARRAY (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(5824 -3536 -710))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-32 (A -64 -68 71) NIL))
                           (V2 (VECTOR-SIGNED-BYTE-32 (A -91 52 -10) NIL)))
                          (CL-ARRAY (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(7772 7172 26460))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-UNSIGNED-BYTE-32 (A 67 44 189) NIL))
                           (V2 (VECTOR-UNSIGNED-BYTE-32 (A 116 163 140) NIL)))
                          (CL-ARRAY (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(5824 -3536 -710))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-64 (A -64 -68 71) NIL))
                           (V2 (VECTOR-SIGNED-BYTE-64 (A -91 52 -10) NIL)))
                          (CL-ARRAY (M* V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(7772 7172 26460))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-UNSIGNED-BYTE-64 (A 67 44 189) NIL))
                           (V2 (VECTOR-UNSIGNED-BYTE-64 (A 116 163 140) NIL)))
                          (CL-ARRAY (M* V1 V2))))))

