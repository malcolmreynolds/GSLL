;; Regression test MATRIX-MEAN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-MEAN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 7.149999883439806d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5)))))
                           (MEAN M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 7.15d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   'DOUBLE-FLOAT
                                                                   :INITIAL-CONTENTS
                                                                   '((-34.5d0
                                                                      8.24d0
                                                                      3.29d0)
                                                                     (-8.93d0
                                                                      34.12d0
                                                                      -6.15d0)
                                                                     (49.27d0
                                                                      -13.49d0
                                                                      32.5d0)))))
                                                             (MEAN M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 9.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '((-64 -68
                                                                      71)
                                                                     (-91 52
                                                                      -10)
                                                                     (73 -5
                                                                      123)))))
                                                             (MEAN M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 132.55555555555554d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (MEAN M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 9.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '((-64 -68
                                                                      71)
                                                                     (-91 52
                                                                      -10)
                                                                     (73 -5
                                                                      123)))))
                                                             (MEAN M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 132.55555555555554d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (MEAN M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 9.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '((-64 -68
                                                                      71)
                                                                     (-91 52
                                                                      -10)
                                                                     (73 -5
                                                                      123)))))
                                                             (MEAN M1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 132.55555555555554d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (MEAN M1))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 9.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '((-64 -68
                                                                      71)
                                                                     (-91 52
                                                                      -10)
                                                                     (73 -5
                                                                      123)))))
                                                             (MEAN M1))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 132.55555555555554d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS
                                             '((67 44 189) (116 163 140)
                                               (161 215 98)))))
                           (MEAN M1)))))

