;; Regression test MATRIX-PRODUCT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-PRODUCT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34803.824 7799.55 -29131.377)
                             (-12393.505 2290.2053 12771.404)
                             (14320.465 -39892.78 4668.254)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5))))
                               (M2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((42.73 -17.24 43.31)
                                               (-16.12 -8.25 21.44)
                                               (-49.08 -39.66 -49.46))))
                               (ANSWER
                                (MAKE-MARRAY 'SINGLE-FLOAT :DIMENSIONS '(3 3)))
                               (S1 19.68)
                               (S2 -5.55))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2 ANSWER S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((-34803.824160000004d0 7799.550047999999d0
                              -29131.375104000002d0)
                             (-12393.505583999999d0 2290.2048959999997d0
                              12771.40488d0)
                             (14320.464911999996d0 -39892.782864d0
                              4668.255407999997d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (M2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((42.73d0 -17.24d0 43.31d0)
                                               (-16.12d0 -8.25d0 21.44d0)
                                               (-49.08d0 -39.66d0 -49.46d0))))
                               (ANSWER
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '(3 3)))
                               (S1 19.68d0)
                               (S2 -5.55d0))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2 ANSWER S1 S2)))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LET ((M1
                                                      (MAKE-MARRAY
                                                       '(COMPLEX SINGLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((-34.5 8.24 3.29 -8.93
                                                          34.12 -6.15)
                                                         (-8.93 34.12 -6.15
                                                          49.27 -13.49 32.5)
                                                         (49.27 -13.49 32.5
                                                          42.73 -17.24
                                                          43.31))))
                                                     (M2
                                                      (MAKE-MARRAY
                                                       '(COMPLEX SINGLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((42.73 -17.24 43.31
                                                          -16.12 -8.25 21.44)
                                                         (-16.12 -8.25 21.44
                                                          -49.08 -39.66 -49.46)
                                                         (-49.08 -39.66 -49.46
                                                          19.68 -5.55 -8.82))))
                                                     (ANSWER
                                                      (MAKE-MARRAY
                                                       '(COMPLEX SINGLE-FLOAT)
                                                       :DIMENSIONS '(3 3)))
                                                     (S1 #C(19.68 -5.55))
                                                     (S2 #C(-5.55 -8.82)))
                                                 (CL-ARRAY
                                                  (MATRIX-PRODUCT M1 M2 ANSWER
                                                                  S1 S2))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LET ((M1
                                                      (MAKE-MARRAY
                                                       '(COMPLEX DOUBLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((-34.5d0 8.24d0 3.29d0
                                                          -8.93d0 34.12d0
                                                          -6.15d0)
                                                         (-8.93d0 34.12d0
                                                          -6.15d0 49.27d0
                                                          -13.49d0 32.5d0)
                                                         (49.27d0 -13.49d0
                                                          32.5d0 42.73d0
                                                          -17.24d0 43.31d0))))
                                                     (M2
                                                      (MAKE-MARRAY
                                                       '(COMPLEX DOUBLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((42.73d0 -17.24d0
                                                          43.31d0 -16.12d0
                                                          -8.25d0 21.44d0)
                                                         (-16.12d0 -8.25d0
                                                          21.44d0 -49.08d0
                                                          -39.66d0 -49.46d0)
                                                         (-49.08d0 -39.66d0
                                                          -49.46d0 19.68d0
                                                          -5.55d0 -8.82d0))))
                                                     (ANSWER
                                                      (MAKE-MARRAY
                                                       '(COMPLEX DOUBLE-FLOAT)
                                                       :DIMENSIONS '(3 3)))
                                                     (S1 #C(19.68d0 -5.55d0))
                                                     (S2 #C(-5.55d0 -8.82d0)))
                                                 (CL-ARRAY
                                                  (MATRIX-PRODUCT M1 M2 ANSWER
                                                                  S1 S2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(23756.895 19926.967 -60376.652))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5))))
                               (V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(42.73 -17.24 43.31)))
                               (ANSWER
                                (MAKE-MARRAY 'SINGLE-FLOAT :DIMENSIONS '3))
                               (S1 -16.12)
                               (S2 -8.25))
                           (CL-ARRAY (MATRIX-PRODUCT M1 V1 ANSWER S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(23756.893524000003d0 19926.966904d0
                           -60376.649164d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(42.73d0 -17.24d0 43.31d0)))
                               (ANSWER
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '3))
                               (S1 -16.12d0)
                               (S2 -8.25d0))
                           (CL-ARRAY (MATRIX-PRODUCT M1 V1 ANSWER S1 S2)))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LET ((M1
                                                      (MAKE-MARRAY
                                                       '(COMPLEX SINGLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((-34.5 8.24 3.29 -8.93
                                                          34.12 -6.15)
                                                         (-8.93 34.12 -6.15
                                                          49.27 -13.49 32.5)
                                                         (49.27 -13.49 32.5
                                                          42.73 -17.24
                                                          43.31))))
                                                     (V1
                                                      (MAKE-MARRAY
                                                       '(COMPLEX SINGLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '(42.73 -17.24 43.31
                                                         -16.12 -8.25 21.44)))
                                                     (ANSWER
                                                      (MAKE-MARRAY
                                                       '(COMPLEX SINGLE-FLOAT)
                                                       :DIMENSIONS '3))
                                                     (S1 #C(-16.12 -8.25))
                                                     (S2 #C(-8.25 21.44)))
                                                 (CL-ARRAY
                                                  (MATRIX-PRODUCT M1 V1 ANSWER
                                                                  S1 S2))))
                       (LISP-UNIT:ASSERT-ERROR 'SIMPLE-ERROR
                                               (LET ((M1
                                                      (MAKE-MARRAY
                                                       '(COMPLEX DOUBLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '((-34.5d0 8.24d0 3.29d0
                                                          -8.93d0 34.12d0
                                                          -6.15d0)
                                                         (-8.93d0 34.12d0
                                                          -6.15d0 49.27d0
                                                          -13.49d0 32.5d0)
                                                         (49.27d0 -13.49d0
                                                          32.5d0 42.73d0
                                                          -17.24d0 43.31d0))))
                                                     (V1
                                                      (MAKE-MARRAY
                                                       '(COMPLEX DOUBLE-FLOAT)
                                                       :INITIAL-CONTENTS
                                                       '(42.73d0 -17.24d0
                                                         43.31d0 -16.12d0
                                                         -8.25d0 21.44d0)))
                                                     (ANSWER
                                                      (MAKE-MARRAY
                                                       '(COMPLEX DOUBLE-FLOAT)
                                                       :DIMENSIONS '3))
                                                     (S1 #C(-16.12d0 -8.25d0))
                                                     (S2 #C(-8.25d0 21.44d0)))
                                                 (CL-ARRAY
                                                  (MATRIX-PRODUCT M1 V1 ANSWER
                                                                  S1 S2)))))

