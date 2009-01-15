;; Regression test MATRIX-PRODUCT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-PRODUCT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((74519.41 -16747.695 61311.69)
                             (27307.273 -6133.5903 -25711.75)
                             (-29088.719 83072.016 -11019.719)))
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
                               (M3
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((19.68 -5.55 -8.82)
                                               (25.37 -30.58 31.67)
                                               (29.36 -33.24 -27.03))))
                               (S1 -41.67)
                               (S2 42.0))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2 M3 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((74519.41329d0 -16747.696061999995d0
                              61311.694176d0)
                             (27307.276670999996d0 -6133.589574d0
                              -25711.752344999997d0)
                             (-29088.718053000033d0 83072.022741d0
                              -11019.721527000016d0)))
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
                               (M3
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((19.68d0 -5.55d0 -8.82d0)
                                               (25.37d0 -30.58d0 31.67d0)
                                               (29.36d0 -33.24d0 -27.03d0))))
                               (S1 -41.67d0)
                               (S2 42.0d0))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2 M3 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(140927.64 -143005.16) #C(72986.305 -201162.86)
                              #C(65956.16 9034.215))
                             (#C(-102500.125 118033.82) #C(-147775.0 58489.473)
                              #C(-9582.236 190719.98))
                             (#C(-16551.412 336505.22) #C(-7668.242 385798.25)
                              #C(45313.527 140641.6))))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29 -8.93 34.12
                                                -6.15)
                                               (-8.93 34.12 -6.15 49.27 -13.49
                                                32.5)
                                               (49.27 -13.49 32.5 42.73 -17.24
                                                43.31))))
                               (M2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((42.73 -17.24 43.31 -16.12 -8.25
                                                21.44)
                                               (-16.12 -8.25 21.44 -49.08
                                                -39.66 -49.46)
                                               (-49.08 -39.66 -49.46 19.68
                                                -5.55 -8.82))))
                               (M3
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((19.68 -5.55 -8.82 25.37 -30.58
                                                31.67)
                                               (25.37 -30.58 31.67 29.36 -33.24
                                                -27.03)
                                               (29.36 -33.24 -27.03 -41.67 42.0
                                                -20.81))))
                               (S1 #C(-41.67 42.0))
                               (S2 #C(42.0 -20.81)))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2 M3 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #2A((#C(140927.638311d0 -143005.14965699997d0)
                              #C(72986.314354d0 -201162.870342d0)
                              #C(65956.156396d0 9034.212785d0))
                             (#C(-102500.143082d0 118033.81826900001d0)
                              #C(-147775.01365700003d0 58489.469011999994d0)
                              #C(-9582.23530800004d0 190719.98257199998d0))
                             (#C(-16551.43698600002d0 336505.21680500003d0)
                              #C(-7668.248943000013d0 385798.22976300004d0)
                              #C(45313.51313299998d0 140641.59851399998d0))))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0 -8.93d0
                                                34.12d0 -6.15d0)
                                               (-8.93d0 34.12d0 -6.15d0 49.27d0
                                                -13.49d0 32.5d0)
                                               (49.27d0 -13.49d0 32.5d0 42.73d0
                                                -17.24d0 43.31d0))))
                               (M2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((42.73d0 -17.24d0 43.31d0
                                                -16.12d0 -8.25d0 21.44d0)
                                               (-16.12d0 -8.25d0 21.44d0
                                                -49.08d0 -39.66d0 -49.46d0)
                                               (-49.08d0 -39.66d0 -49.46d0
                                                19.68d0 -5.55d0 -8.82d0))))
                               (M3
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((19.68d0 -5.55d0 -8.82d0 25.37d0
                                                -30.58d0 31.67d0)
                                               (25.37d0 -30.58d0 31.67d0
                                                29.36d0 -33.24d0 -27.03d0)
                                               (29.36d0 -33.24d0 -27.03d0
                                                -41.67d0 42.0d0 -20.81d0))))
                               (S1 #C(-41.67d0 42.0d0))
                               (S2 #C(42.0d0 -20.81d0)))
                           (CL-ARRAY (MATRIX-PRODUCT M1 M2 M3 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(72971.1 60998.137 -184676.98))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5))))
                               (V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(42.73 -17.24 43.31)))
                               (V2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-16.12 -8.25 21.44)))
                               (S1 -49.08)
                               (S2 -39.66))
                           (CL-ARRAY (MATRIX-PRODUCT M1 V1 V2 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(72971.10171599999d0 60998.13393599999d0
                           -184676.981676d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(42.73d0 -17.24d0 43.31d0)))
                               (V2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-16.12d0 -8.25d0 21.44d0)))
                               (S1 -49.08d0)
                               (S2 -39.66d0))
                           (CL-ARRAY (MATRIX-PRODUCT M1 V1 V2 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(124163.57 -3332.2988) #C(119793.48 -166378.0)
                           #C(-189845.23 -81764.12)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29 -8.93 34.12
                                                -6.15)
                                               (-8.93 34.12 -6.15 49.27 -13.49
                                                32.5)
                                               (49.27 -13.49 32.5 42.73 -17.24
                                                43.31))))
                               (V1
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(42.73 -17.24 43.31 -16.12 -8.25
                                               21.44)))
                               (V2
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-16.12 -8.25 21.44 -49.08 -39.66
                                               -49.46)))
                               (S1 #C(-49.08 -39.66))
                               (S2 #C(-39.66 -49.46)))
                           (CL-ARRAY (MATRIX-PRODUCT M1 V1 V2 S1 S2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(124163.58013199999d0 -3332.2974459999896d0)
                           #C(119793.47618999999d0 -166378.00423d0)
                           #C(-189845.21679399998d0 -81764.10481799999d0)))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0 -8.93d0
                                                34.12d0 -6.15d0)
                                               (-8.93d0 34.12d0 -6.15d0 49.27d0
                                                -13.49d0 32.5d0)
                                               (49.27d0 -13.49d0 32.5d0 42.73d0
                                                -17.24d0 43.31d0))))
                               (V1
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0)))
                               (V2
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-16.12d0 -8.25d0 21.44d0
                                               -49.08d0 -39.66d0 -49.46d0)))
                               (S1 #C(-49.08d0 -39.66d0))
                               (S2 #C(-39.66d0 -49.46d0)))
                           (CL-ARRAY (MATRIX-PRODUCT M1 V1 V2 S1 S2))))))

