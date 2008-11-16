;; Regression test MATRIX-PRODUCT-HERMITIAN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-PRODUCT-HERMITIAN
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((M1
                                                  (MATRIX-COMPLEX-SINGLE-FLOAT
                                                   (A
                                                    (-34.5 8.24 3.29 -8.93
                                                     34.12 -6.15)
                                                    (-8.93 34.12 -6.15 49.27
                                                     -13.49 32.5)
                                                    (49.27 -13.49 32.5 42.73
                                                     -17.24 43.31))
                                                   NIL))
                                                 (M2
                                                  (MATRIX-COMPLEX-SINGLE-FLOAT
                                                   (A
                                                    (42.73 -17.24 43.31 -16.12
                                                     -8.25 21.44)
                                                    (-16.12 -8.25 21.44 -49.08
                                                     -39.66 -49.46)
                                                    (-49.08 -39.66 -49.46 19.68
                                                     -5.55 -8.82))
                                                   NIL))
                                                 (ANSWER
                                                  (MATRIX-COMPLEX-SINGLE-FLOAT
                                                   '(3 3) NIL))
                                                 (S1 #C(19.68 -5.55))
                                                 (S2 #C(-5.55 -8.82)))
                                                (CL-ARRAY
                                                 (MATRIX-PRODUCT-HERMITIAN M1
                                                                           M2
                                                                           ANSWER
                                                                           S1
                                                                           S2))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((M1
                                                  (MATRIX-COMPLEX-DOUBLE-FLOAT
                                                   (A
                                                    (-34.5d0 8.24d0 3.29d0
                                                     -8.93d0 34.12d0 -6.15d0)
                                                    (-8.93d0 34.12d0 -6.15d0
                                                     49.27d0 -13.49d0 32.5d0)
                                                    (49.27d0 -13.49d0 32.5d0
                                                     42.73d0 -17.24d0 43.31d0))
                                                   NIL))
                                                 (M2
                                                  (MATRIX-COMPLEX-DOUBLE-FLOAT
                                                   (A
                                                    (42.73d0 -17.24d0 43.31d0
                                                     -16.12d0 -8.25d0 21.44d0)
                                                    (-16.12d0 -8.25d0 21.44d0
                                                     -49.08d0 -39.66d0
                                                     -49.46d0)
                                                    (-49.08d0 -39.66d0 -49.46d0
                                                     19.68d0 -5.55d0 -8.82d0))
                                                   NIL))
                                                 (ANSWER
                                                  (MATRIX-COMPLEX-DOUBLE-FLOAT
                                                   '(3 3) NIL))
                                                 (S1 #C(19.68d0 -5.55d0))
                                                 (S2 #C(-5.55d0 -8.82d0)))
                                                (CL-ARRAY
                                                 (MATRIX-PRODUCT-HERMITIAN M1
                                                                           M2
                                                                           ANSWER
                                                                           S1
                                                                           S2))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((M1
                                                  (MATRIX-COMPLEX-SINGLE-FLOAT
                                                   (A
                                                    (-34.5 8.24 3.29 -8.93
                                                     34.12 -6.15)
                                                    (-8.93 34.12 -6.15 49.27
                                                     -13.49 32.5)
                                                    (49.27 -13.49 32.5 42.73
                                                     -17.24 43.31))
                                                   NIL))
                                                 (V1
                                                  (VECTOR-COMPLEX-SINGLE-FLOAT
                                                   (A 42.73 -17.24 43.31 -16.12
                                                      -8.25 21.44)
                                                   NIL))
                                                 (ANSWER
                                                  (VECTOR-COMPLEX-SINGLE-FLOAT
                                                   '3 NIL))
                                                 (S1 #C(-16.12 -8.25))
                                                 (S2 #C(-8.25 21.44)))
                                                (CL-ARRAY
                                                 (MATRIX-PRODUCT-HERMITIAN M1
                                                                           V1
                                                                           ANSWER
                                                                           S1
                                                                           S2))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LETM
                                                ((M1
                                                  (MATRIX-COMPLEX-DOUBLE-FLOAT
                                                   (A
                                                    (-34.5d0 8.24d0 3.29d0
                                                     -8.93d0 34.12d0 -6.15d0)
                                                    (-8.93d0 34.12d0 -6.15d0
                                                     49.27d0 -13.49d0 32.5d0)
                                                    (49.27d0 -13.49d0 32.5d0
                                                     42.73d0 -17.24d0 43.31d0))
                                                   NIL))
                                                 (V1
                                                  (VECTOR-COMPLEX-DOUBLE-FLOAT
                                                   (A 42.73d0 -17.24d0 43.31d0
                                                      -16.12d0 -8.25d0 21.44d0)
                                                   NIL))
                                                 (ANSWER
                                                  (VECTOR-COMPLEX-DOUBLE-FLOAT
                                                   '3 NIL))
                                                 (S1 #C(-16.12d0 -8.25d0))
                                                 (S2 #C(-8.25d0 21.44d0)))
                                                (CL-ARRAY
                                                 (MATRIX-PRODUCT-HERMITIAN M1
                                                                           V1
                                                                           ANSWER
                                                                           S1
                                                                           S2)))))

