;; Regression test AXPY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST AXPY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(1400.77 -284.0684 -147.7214 397.47382 -1369.3191
                           235.659 -1932.6083 485.93335))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-SINGLE-FLOAT
                             (A -34.5 8.24 3.29 -8.93 34.12 -6.15 49.27 -13.49)
                             NIL))
                           (V2
                            (VECTOR-SINGLE-FLOAT
                             (A 32.5 42.73 -17.24 43.31 -16.12 -8.25 21.44
                                -49.08)
                             NIL))
                           (SCALAR -39.66))
                          (CL-ARRAY (AXPY SCALAR V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(1400.77d0 -284.06839999999994d0 -147.7214d0
                           397.4738d0 -1369.3191999999997d0 235.659d0
                           -1932.6082d0 485.93339999999995d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((V1
                            (VECTOR-DOUBLE-FLOAT
                             (A -34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0
                                49.27d0 -13.49d0)
                             NIL))
                           (V2
                            (VECTOR-DOUBLE-FLOAT
                             (A 32.5d0 42.73d0 -17.24d0 43.31d0 -16.12d0
                                -8.25d0 21.44d0 -49.08d0)
                             NIL))
                           (SCALAR -39.66d0))
                          (CL-ARRAY (AXPY SCALAR V1 V2)))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((V1
                                                  (VECTOR-COMPLEX-SINGLE-FLOAT
                                                   (A -34.5 8.24 3.29 -8.93
                                                      34.12 -6.15 49.27 -13.49
                                                      32.5 42.73 -17.24 43.31
                                                      -16.12 -8.25 21.44
                                                      -49.08)
                                                   NIL))
                                                 (V2
                                                  (VECTOR-COMPLEX-SINGLE-FLOAT
                                                   (A 32.5 42.73 -17.24 43.31
                                                      -16.12 -8.25 21.44 -49.08
                                                      -39.66 -49.46 19.68 -5.55
                                                      -8.82 25.37 -30.58 31.67)
                                                   NIL))
                                                 (SCALAR #C(-39.66 -49.46)))
                                                (CL-ARRAY
                                                 (AXPY SCALAR V1 V2))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((V1
                                                  (VECTOR-COMPLEX-DOUBLE-FLOAT
                                                   (A -34.5d0 8.24d0 3.29d0
                                                      -8.93d0 34.12d0 -6.15d0
                                                      49.27d0 -13.49d0 32.5d0
                                                      42.73d0 -17.24d0 43.31d0
                                                      -16.12d0 -8.25d0 21.44d0
                                                      -49.08d0)
                                                   NIL))
                                                 (V2
                                                  (VECTOR-COMPLEX-DOUBLE-FLOAT
                                                   (A 32.5d0 42.73d0 -17.24d0
                                                      43.31d0 -16.12d0 -8.25d0
                                                      21.44d0 -49.08d0 -39.66d0
                                                      -49.46d0 19.68d0 -5.55d0
                                                      -8.82d0 25.37d0 -30.58d0
                                                      31.67d0)
                                                   NIL))
                                                 (SCALAR
                                                  #C(-39.66d0 -49.46d0)))
                                                (CL-ARRAY
                                                 (AXPY SCALAR V1 V2)))))

