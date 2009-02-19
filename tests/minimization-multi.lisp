;; Regression test MINIMIZATION-MULTI for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MINIMIZATION-MULTI
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9920430849306285d0 1.9969168063253164d0
                              30.000823246638923d0)
                        (MULTIPLE-VALUE-LIST
                         (MULTIMIN-EXAMPLE-NO-DERIVATIVE +SIMPLEX-NELDER-MEAD-on2+
                                                         NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9999999999999997d0 2.0d0 30.0d0)
                        (MULTIPLE-VALUE-LIST
                         (MULTIMIN-EXAMPLE-DERIVATIVE
                          +CONJUGATE-FLETCHER-REEVES+ NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9999999999999997d0 2.0d0 30.0d0)
                        (MULTIPLE-VALUE-LIST
                         (MULTIMIN-EXAMPLE-DERIVATIVE +CONJUGATE-POLAK-RIBIERE+
                                                      NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9999999999999997d0 2.0d0 30.0d0)
                        (MULTIPLE-VALUE-LIST
                         (MULTIMIN-EXAMPLE-DERIVATIVE +VECTOR-BFGS+ NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9999999999998208d0 1.9999999999995521d0 30.0d0)
                        (MULTIPLE-VALUE-LIST
                         (MULTIMIN-EXAMPLE-DERIVATIVE +VECTOR-BFGS2+ NIL))))

