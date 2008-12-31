;; Regression test POLYNOMIAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST POLYNOMIAL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 2.5d0 7.2d0 32.7d0 91.0d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((XA
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(0.0d0 1.0d0 2.0d0 3.0d0)))
                               (YA
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(2.5d0 7.2d0 32.7d0 91.0d0)))
                               (DD (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (DIVIDED-DIFFERENCE DD XA YA)
                           (LIST
                            (POLYNOMIAL-EVAL-DIVIDED-DIFFERENCE DD XA 0.0d0)
                            (POLYNOMIAL-EVAL-DIVIDED-DIFFERENCE DD XA 1.0d0)
                            (POLYNOMIAL-EVAL-DIVIDED-DIFFERENCE DD XA 2.0d0)
                            (POLYNOMIAL-EVAL-DIVIDED-DIFFERENCE DD XA
                                                                3.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 2.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((VEC
                                                                  (MAKE-MARRAY
                                                                   'DOUBLE-FLOAT
                                                                   :INITIAL-CONTENTS
                                                                   '(1.0d0
                                                                     2.0d0
                                                                     3.0d0))))
                                                             (POLYNOMIAL-EVAL
                                                              VEC -1.0d0))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST (LIST) (LIST))
                                                          (MULTIPLE-VALUE-LIST
                                                           (SOLVE-QUADRATIC
                                                            1.0d0 0.0d0
                                                            1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (SOLVE-QUADRATIC
                                                            1.0d0 -2.0d0
                                                            1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #C(1.0d0 0.0d0) #C(1.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 1.0d0 -2.0d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -3.000000000000001d0 1.9999999999999996d0
                              7.000000000000001d0)
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC -6.0d0 -13.0d0 42.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-5.551115123125783d-17 -0.9999999999999999d0)
                              #C(-5.551115123125783d-17 0.9999999999999999d0)
                              #C(1.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC-COMPLEX -1.0d0 1.0d0 -1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-0.8090169943749477d0 0.5877852522924734d0)
                           #C(-0.8090169943749477d0 -0.5877852522924734d0)
                           #C(0.3090169943749475d0 0.951056516295153d0)
                           #C(0.3090169943749475d0 -0.951056516295153d0)
                           #C(0.9999999999999999d0 0.0d0)))
                        (MULTIPLE-VALUE-LIST
                         (CL-ARRAY
                          (POLYNOMIAL-SOLVE
                           (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                        '(-1.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                                          1.0d0)))))))

