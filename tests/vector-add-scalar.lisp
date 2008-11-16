;; Regression test VECTOR-ADD-SCALAR for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-ADD-SCALAR
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-16.31 26.43 21.48))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SINGLE-FLOAT (A -34.5 8.24 3.29) NIL)))
                          (CL-ARRAY (M+C V1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-16.31d0 26.43d0 21.48d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL)))
                          (CL-ARRAY (M+C V1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-45 -49 89))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+C V1
                                                                  18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(85 62 207))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+C V1
                                                                  18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-45 -49 89))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+C V1
                                                                  18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(85 62 207))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+C V1
                                                                  18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-45 -49 89))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+C V1
                                                                  18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(85 62 207))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+C V1
                                                                  18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-45 -49 89))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+C V1
                                                                  18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(85 62 207))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M+C V1
                                                                  18.19d0))))))

