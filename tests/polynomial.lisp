;; Regression test POLYNOMIAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST POLYNOMIAL
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 2.5d0 7.2d0 32.7d0 91.0d0))
                        (MULTIPLE-VALUE-LIST
                         (LET* ((XA
                                 (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                              '(0.0d0 1.0d0 2.0d0 3.0d0)))
                                (DD
                                 (DIVIDED-DIFFERENCE XA
                                                     (MAKE-MARRAY 'DOUBLE-FLOAT
                                                                  :INITIAL-CONTENTS
                                                                  '(2.5d0 7.2d0
                                                                    32.7d0
                                                                    91.0d0)))))
                           (LIST (EVALUATE XA 0.0d0 :DIVIDED-DIFFERENCE DD)
                                 (EVALUATE XA 1.0d0 :DIVIDED-DIFFERENCE DD)
                                 (EVALUATE XA 2.0d0 :DIVIDED-DIFFERENCE DD)
                                 (EVALUATE XA 3.0d0 :DIVIDED-DIFFERENCE DD)))))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST 2.0d0)
                                                         (MULTIPLE-VALUE-LIST
                                                          (LET ((VEC
                                                                 (MAKE-MARRAY
                                                                  'DOUBLE-FLOAT
                                                                  :INITIAL-CONTENTS
                                                                  '(1.0d0 2.0d0
                                                                    3.0d0))))
                                                            (EVALUATE VEC
                                                                      -1.0d0))))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST (LIST) (LIST))
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           1.0d0 0.0d0 1.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST 1.0d0 1.0d0)
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           1.0d0 -2.0d0
                                                           1.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(1.0d0 0.0d0) #C(1.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 1.0d0 -2.0d0 1.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST -3.000000000000001d0 1.9999999999999996d0
                              7.000000000000001d0)
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC -6.0d0 -13.0d0 42.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-5.551115123125783d-17 -0.9999999999999999d0)
                              #C(-5.551115123125783d-17 0.9999999999999999d0)
                              #C(1.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC-COMPLEX -1.0d0 1.0d0 -1.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-0.8090169943749477d0 0.5877852522924734d0)
                           #C(-0.8090169943749477d0 -0.5877852522924734d0)
                           #C(0.3090169943749475d0 0.951056516295153d0)
                           #C(0.3090169943749475d0 -0.951056516295153d0)
                           #C(0.9999999999999999d0 0.0d0)))
                        (MULTIPLE-VALUE-LIST
                         (COPY
                          (POLYNOMIAL-SOLVE
                           (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                        '(-1.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                                          1.0d0)))
                          'ARRAY)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST 1.3250000029802322d0)
                        (MULTIPLE-VALUE-LIST
                         (EVALUATE
                          (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                       '(1 0.5 0.3))
                          0.5d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                         (MULTIPLE-VALUE-LIST
                                                          (EVALUATE
                                                           (MAKE-MARRAY
                                                            'DOUBLE-FLOAT
                                                            :INITIAL-CONTENTS
                                                            '(1 -1 1 -1 1 -1 1
                                                              -1 1 -1 1))
                                                           1.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST (LIST) (LIST))
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           4.0d0 -20.0d0
                                                           26.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST 2.5d0 2.5d0)
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           4.0d0 -20.0d0
                                                           25.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST 1.5d0 3.5d0)
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           4.0d0 -20.0d0
                                                           21.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST -1.75d0 -0.0d0)
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           4.0d0 7.0d0 0.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST -2.0d0 2.0d0)
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           5.0d0 0.0d0
                                                           -20.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST 7.0d0 (LIST))
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           0.0d0 3.0d0
                                                           -21.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST (LIST) (LIST))
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC
                                                           0.0d0 0.0d0 1.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST 3.0d0 (LIST) (LIST))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC 0.0d0 0.0d0 -27.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST 17.0d0 17.0d0 17.0d0)
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC -51.0d0 867.0d0 -4913.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST 17.0d0 17.0d0 23.0d0)
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC -57.0d0 1071.0d0 -6647.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST -23.0d0 17.0d0 17.0d0)
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC -11.0d0 -493.0d0 6647.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST 16.999999999999993d0 31.000000000000004d0 95.0d0)
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC -143.0d0 5087.0d0 -50065.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST -16.999999999999993d0 30.999999999999996d0
                              95.0d0)
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC -109.0d0 803.0d0 50065.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(2.5d0 -0.5d0) #C(2.5d0 0.5d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 4.0d0 -20.0d0 26.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(2.5d0 0.0d0) #C(2.5d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 4.0d0 -20.0d0 25.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(1.5d0 0.0d0) #C(3.5d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 4.0d0 -20.0d0 21.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-1.75d0 0.0d0) #C(-0.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 4.0d0 7.0d0 0.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-2.0d0 0.0d0) #C(2.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 5.0d0 0.0d0 -20.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-0.0d0 -2.0d0) #C(-0.0d0 2.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 5.0d0 0.0d0 20.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(7.0d0 0.0d0) (LIST))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-QUADRATIC-COMPLEX 0.0d0 3.0d0 -21.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST (LIST) (LIST))
                                                         (MULTIPLE-VALUE-LIST
                                                          (SOLVE-QUADRATIC-COMPLEX
                                                           0.0d0 0.0d0 1.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-1.5d0 -2.598076211353316d0)
                              #C(-1.5d0 2.598076211353316d0) #C(3.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC-COMPLEX 0.0d0 0.0d0 -27.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-2.9999999999999996d0 0.0d0)
                              #C(1.9999999999999998d0 -2.9999999999999996d0)
                              #C(1.9999999999999998d0 2.9999999999999996d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC-COMPLEX -1.0d0 1.0d0 39.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(17.0d0 0.0d0) #C(17.0d0 0.0d0)
                              #C(17.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC-COMPLEX -51.0d0 867.0d0 -4913.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(17.0d0 0.0d0) #C(17.0d0 0.0d0)
                              #C(23.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC-COMPLEX -57.0d0 1071.0d0 -6647.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-23.0d0 0.0d0) #C(17.0d0 0.0d0)
                              #C(17.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC-COMPLEX -11.0d0 -493.0d0 6647.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST #C(16.999999999999993d0 0.0d0)
                              #C(31.000000000000004d0 0.0d0) #C(95.0d0 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (SOLVE-CUBIC-COMPLEX -143.0d0 5087.0d0 -50065.0d0)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(0.9999999999999969d0 0.0d0)
                           #C(2.000000000000055d0 0.0d0)
                           #C(2.9999999999998614d0 0.0d0)
                           #C(4.000000000000085d0 0.0d0)
                           #C(4.9999999999999964d0 0.0d0)))
                        (MULTIPLE-VALUE-LIST
                         (COPY
                          (POLYNOMIAL-SOLVE
                           (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                        '(-120 274 -225 85 -15 1.0)))
                          'ARRAY)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(-0.8660254037844393d0 0.49999999999999983d0)
                           #C(-0.8660254037844393d0 -0.49999999999999983d0)
                           #C(-0.5000000000000001d0 0.866025403784439d0)
                           #C(-0.5000000000000001d0 -0.866025403784439d0)
                           #C(0.4999999999999999d0 0.8660254037844388d0)
                           #C(0.4999999999999999d0 -0.8660254037844388d0)
                           #C(0.8660254037844388d0 0.4999999999999996d0)
                           #C(0.8660254037844388d0 -0.4999999999999996d0)))
                        (MULTIPLE-VALUE-LIST
                         (COPY
                          (POLYNOMIAL-SOLVE
                           (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                        '(1 0 0 0 1 0 0 0 1)))
                          'ARRAY)))
                       (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST
                          #(0.7300000190734863d0 0.4691357779404764d0
                            -0.043473715982275014d0 0.026868123371519625d0
                            -0.0032293849799736804d0 0.0061276698885453776d0
                            -0.006454056591950834d0)
                          #(0.7300000190734863d0 1.1100000143051147d0
                            1.4900000095367432d0 1.840000033378601d0
                            2.299999952316284d0 2.4100000858306885d0
                            3.069999933242798d0)
                          #(0.7300000190734866d0 1.110000014305115d0
                            1.4900000095367432d0 1.840000033378601d0
                            2.299999952316284d0 2.4100000858306885d0
                            3.069999933242798d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET* ((XA
                                 (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                              '(0.16 0.97 1.94 2.74 3.58 3.73
                                                4.7)))
                                (YA
                                 (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                              '(0.73 1.11 1.49 1.84 2.3 2.41
                                                3.07)))
                                (DD (DIVIDED-DIFFERENCE XA YA)))
                           (LIST (COPY DD 'ARRAY)
                                 (MAP 'VECTOR
                                      (LAMBDA (X)
                                        (EVALUATE XA X :DIVIDED-DIFFERENCE DD))
                                      (COPY XA 'ARRAY))
                                 (MAP 'VECTOR
                                      (LAMBDA (X)
                                        (EVALUATE
                                         (TAYLOR-DIVIDED-DIFFERENCE 1.5d0 DD
                                                                    XA)
                                         (- X 1.5d0)))
                                      (COPY XA 'ARRAY)))))))

