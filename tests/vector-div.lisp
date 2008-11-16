;; Regression test VECTOR-DIV for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-DIV
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(3.8633816 0.24150059 -0.5349593))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SINGLE-FLOAT (A -34.5 8.24 3.29) NIL))
                           (V2
                            (VECTOR-SINGLE-FLOAT (A -8.93 34.12 -6.15) NIL)))
                          (CL-ARRAY (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(3.8633818589025757d0 0.2415005861664713d0
                           -0.5349593495934959d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL))
                           (V2
                            (VECTOR-DOUBLE-FLOAT (A -8.93d0 34.12d0 -6.15d0)
                                                 NIL)))
                          (CL-ARRAY (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 -1 -7))
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
                                                             (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 1))
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
                                                             (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 -1 -7))
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
                                                             (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 1))
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
                                                             (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 -1 -7))
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
                                                             (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 1))
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
                                                             (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 -1 -7))
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
                                                             (M/ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 0 1))
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
                                                             (M/ V1 V2))))))

