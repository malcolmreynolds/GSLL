;; Regression test VECTOR-SWAP for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-SWAP
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(-8.93 34.12 -6.15) #(-34.5 8.24 3.29)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SINGLE-FLOAT (A -34.5 8.24 3.29) NIL))
                           (V2
                            (VECTOR-SINGLE-FLOAT (A -8.93 34.12 -6.15) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST #(-8.93d0 34.12d0 -6.15d0)
                               #(-34.5d0 8.24d0 3.29d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL))
                           (V2
                            (VECTOR-DOUBLE-FLOAT (A -8.93d0 34.12d0 -6.15d0)
                                                 NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST
                          #(#C(-8.93 34.12) #C(-6.15 49.27) #C(-13.49 32.5))
                          #(#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15))))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A -34.5 8.24 3.29 -8.93 34.12 -6.15) NIL))
                           (V2
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A -8.93 34.12 -6.15 49.27 -13.49 32.5) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST
                          #(#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0)
                            #C(-13.49d0 32.5d0))
                          #(#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0)
                            #C(34.12d0 -6.15d0))))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0)
                             NIL))
                           (V2
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A -8.93d0 34.12d0 -6.15d0 49.27d0 -13.49d0
                                32.5d0)
                             NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(-91 52 -10) #(-64 -68 71)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-8 (A -64 -68 71) NIL))
                           (V2 (VECTOR-SIGNED-BYTE-8 (A -91 52 -10) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(116 163 140) #(67 44 189)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-UNSIGNED-BYTE-8 (A 67 44 189) NIL))
                           (V2 (VECTOR-UNSIGNED-BYTE-8 (A 116 163 140) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(-91 52 -10) #(-64 -68 71)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-16 (A -64 -68 71) NIL))
                           (V2 (VECTOR-SIGNED-BYTE-16 (A -91 52 -10) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(116 163 140) #(67 44 189)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-UNSIGNED-BYTE-16 (A 67 44 189) NIL))
                           (V2 (VECTOR-UNSIGNED-BYTE-16 (A 116 163 140) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(-91 52 -10) #(-64 -68 71)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-32 (A -64 -68 71) NIL))
                           (V2 (VECTOR-SIGNED-BYTE-32 (A -91 52 -10) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(116 163 140) #(67 44 189)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-UNSIGNED-BYTE-32 (A 67 44 189) NIL))
                           (V2 (VECTOR-UNSIGNED-BYTE-32 (A 116 163 140) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(-91 52 -10) #(-64 -68 71)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SIGNED-BYTE-64 (A -64 -68 71) NIL))
                           (V2 (VECTOR-SIGNED-BYTE-64 (A -91 52 -10) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(116 163 140) #(67 44 189)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-UNSIGNED-BYTE-64 (A 67 44 189) NIL))
                           (V2 (VECTOR-UNSIGNED-BYTE-64 (A 116 163 140) NIL)))
                          (SWAP V2 V1) (LIST (CL-ARRAY V1) (CL-ARRAY V2))))))

