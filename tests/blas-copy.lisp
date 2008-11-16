;; Regression test BLAS-COPY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST BLAS-COPY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-34.5 8.24 3.29))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1 (VECTOR-SINGLE-FLOAT (A -34.5 8.24 3.29) NIL))
                           (V2 (VECTOR-SINGLE-FLOAT '3 NIL)))
                          (BLAS-COPY V1 V2) (CL-ARRAY V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-34.5d0 8.24d0 3.29d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT (A -34.5d0 8.24d0 3.29d0)
                                                 NIL))
                           (V2 (VECTOR-DOUBLE-FLOAT '3 NIL)))
                          (BLAS-COPY V1 V2) (CL-ARRAY V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-SINGLE-FLOAT
                             (A -34.5 8.24 3.29 -8.93 34.12 -6.15) NIL))
                           (V2 (VECTOR-COMPLEX-SINGLE-FLOAT '3 NIL)))
                          (BLAS-COPY V1 V2) (CL-ARRAY V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0)
                           #C(34.12d0 -6.15d0)))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-COMPLEX-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0)
                             NIL))
                           (V2 (VECTOR-COMPLEX-DOUBLE-FLOAT '3 NIL)))
                          (BLAS-COPY V1 V2) (CL-ARRAY V2)))))

