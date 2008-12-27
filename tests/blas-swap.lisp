;; Regression test BLAS-SWAP for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST BLAS-SWAP
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST #(-8.93 34.12 -6.15) #(-34.5 8.24 3.29)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29)))
                               (V2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-8.93 34.12 -6.15))))
                           (BLAS-SWAP V2 V1)
                           (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST #(-8.93d0 34.12d0 -6.15d0)
                               #(-34.5d0 8.24d0 3.29d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0)))
                               (V2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-8.93d0 34.12d0 -6.15d0))))
                           (BLAS-SWAP V2 V1)
                           (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST
                          #(#C(-8.93 34.12) #C(-6.15 49.27) #C(-13.49 32.5))
                          #(#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15))))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29 -8.93 34.12
                                               -6.15)))
                               (V2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-8.93 34.12 -6.15 49.27 -13.49
                                               32.5))))
                           (BLAS-SWAP V2 V1)
                           (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST
                          #(#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0)
                            #C(-13.49d0 32.5d0))
                          #(#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0)
                            #C(34.12d0 -6.15d0))))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0 -8.93d0
                                               34.12d0 -6.15d0)))
                               (V2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-8.93d0 34.12d0 -6.15d0 49.27d0
                                               -13.49d0 32.5d0))))
                           (BLAS-SWAP V2 V1)
                           (LIST (CL-ARRAY V1) (CL-ARRAY V2))))))

