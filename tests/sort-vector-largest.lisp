;; Regression test SORT-VECTOR-LARGEST for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SORT-VECTOR-LARGEST
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(49.27 34.12 8.24))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-SINGLE-FLOAT
                             (A -34.5 8.24 3.29 -8.93 34.12 -6.15 49.27 -13.49)
                             NIL))
                           (V2
                            (VECTOR-SINGLE-FLOAT (A 32.5 42.73 -17.24) NIL)))
                          (CL-ARRAY (SORT-VECTOR-LARGEST V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(49.27d0 34.12d0 8.24d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0
                                49.27d0 -13.49d0)
                             NIL))
                           (V2
                            (VECTOR-DOUBLE-FLOAT (A 32.5d0 42.73d0 -17.24d0)
                                                 NIL)))
                          (CL-ARRAY (SORT-VECTOR-LARGEST V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(73 71 52))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -64 -68 71
                                                                  -91 52 -10 73
                                                                  -5)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A 123 32 28)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (SORT-VECTOR-LARGEST
                                                              V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(215 189 163))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 67 44 189 116
                                                                  163 140 161
                                                                  215)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 98 28 10)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (SORT-VECTOR-LARGEST
                                                              V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(73 71 52))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A -64 -68 71
                                                                  -91 52 -10 73
                                                                  -5)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A 123 32 28)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (SORT-VECTOR-LARGEST
                                                              V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(215 189 163))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 67 44 189 116
                                                                  163 140 161
                                                                  215)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 98 28 10)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (SORT-VECTOR-LARGEST
                                                              V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(73 71 52))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A -64 -68 71
                                                                  -91 52 -10 73
                                                                  -5)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A 123 32 28)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (SORT-VECTOR-LARGEST
                                                              V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(215 189 163))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 67 44 189 116
                                                                  163 140 161
                                                                  215)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 98 28 10)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (SORT-VECTOR-LARGEST
                                                              V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(73 71 52))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A -64 -68 71
                                                                  -91 52 -10 73
                                                                  -5)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A 123 32 28)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (SORT-VECTOR-LARGEST
                                                              V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(215 189 163))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 67 44 189 116
                                                                  163 140 161
                                                                  215)
                                                               NIL))
                                                             (V2
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 98 28 10)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (SORT-VECTOR-LARGEST
                                                              V2 V1))))))

