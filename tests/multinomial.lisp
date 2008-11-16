;; Regression test MULTINOMIAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MULTINOMIAL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(5 0 1 2))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((RNG
                                                              (RANDOM-NUMBER-GENERATOR
                                                               *MT19937* 0))
                                                             (P
                                                              (VECTOR-DOUBLE-FLOAT
                                                               (A 0.1d0 0.2d0
                                                                  0.3d0
                                                                  0.4d0)))
                                                             (N
                                                              (VECTOR-SIGNED-BYTE-32
                                                               4)))
                                                            (MULTINOMIAL RNG 8
                                                                         P N)
                                                            (CL-ARRAY N))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 8.064000000000026d-5)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((P
                            (VECTOR-DOUBLE-FLOAT (A 0.1d0 0.2d0 0.3d0 0.4d0)))
                           (N (VECTOR-SIGNED-BYTE-32 (A 5 0 1 2))))
                          (MULTINOMIAL-PDF P N))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -9.425515753641212d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((P
                            (VECTOR-DOUBLE-FLOAT (A 0.1d0 0.2d0 0.3d0 0.4d0)))
                           (N (VECTOR-SIGNED-BYTE-32 (A 5 0 1 2))))
                          (MULTINOMIAL-LOG-PDF P N)))))

