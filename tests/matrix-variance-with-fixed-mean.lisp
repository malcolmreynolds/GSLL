;; Regression test MATRIX-VARIANCE-WITH-FIXED-MEAN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-VARIANCE-WITH-FIXED-MEAN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 639.5971092940438d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SINGLE-FLOAT
                             (A (-34.5 8.24 3.29) (-8.93 34.12 -6.15)
                                (49.27 -13.49 32.5))
                             NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 639.5971111111112d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-DOUBLE-FLOAT
                             (A (-34.5d0 8.24d0 3.29d0)
                                (-8.93d0 34.12d0 -6.15d0)
                                (49.27d0 -13.49d0 32.5d0))
                             NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 4955.555555555556d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-8
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2820.246913580247d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-8
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 4955.555555555556d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-16
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2820.246913580247d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-16
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 4955.555555555556d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-32
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2820.246913580247d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-32
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 4955.555555555556d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-SIGNED-BYTE-64
                             (A (-64 -68 71) (-91 52 -10) (73 -5 123)) NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2820.246913580247d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((M1
                            (MATRIX-UNSIGNED-BYTE-64
                             (A (67 44 189) (116 163 140) (161 215 98)) NIL)))
                          (VARIANCE-WITH-FIXED-MEAN M1 (MEAN M1))))))

