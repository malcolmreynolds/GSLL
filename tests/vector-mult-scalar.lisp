;; Regression test VECTOR-MULT-SCALAR for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-MULT-SCALAR
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-47.955 11.4536 4.5731))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SINGLE-FLOAT (A -34.5 8.24 3.29) NIL)))
                          (CL-ARRAY (M*C V1 1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-47.955d0 11.4536d0 4.5731d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL)))
                          (CL-ARRAY (M*C V1 1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-88 -94 98))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M*C V1
                                                                  1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(93 61 6))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M*C V1
                                                                  1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-88 -94 98))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M*C V1
                                                                  1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(93 61 262))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M*C V1
                                                                  1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-88 -94 98))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M*C V1
                                                                  1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(93 61 262))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M*C V1
                                                                  1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-88 -94 98))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M*C V1
                                                                  1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(93 61 262))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (CL-ARRAY
                                                             (M*C V1
                                                                  1.39d0))))))

