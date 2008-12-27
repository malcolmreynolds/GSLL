;; Regression test BLAS-COPY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST BLAS-COPY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-34.5 8.24 3.29))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29)))
                               (V2 (MAKE-MARRAY 'SINGLE-FLOAT :DIMENSIONS '3)))
                           (BLAS-COPY V1 V2)
                           (CL-ARRAY V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-34.5d0 8.24d0 3.29d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0)))
                               (V2 (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '3)))
                           (BLAS-COPY V1 V2)
                           (CL-ARRAY V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29 -8.93 34.12
                                               -6.15)))
                               (V2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :DIMENSIONS '3)))
                           (BLAS-COPY V1 V2)
                           (CL-ARRAY V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0)
                           #C(34.12d0 -6.15d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0 -8.93d0
                                               34.12d0 -6.15d0)))
                               (V2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :DIMENSIONS '3)))
                           (BLAS-COPY V1 V2)
                           (CL-ARRAY V2)))))

