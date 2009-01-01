;; Regression test COLUMN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST COLUMN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(8.24 34.12 -13.49))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5 8.24 3.29)
                                               (-8.93 34.12 -6.15)
                                               (49.27 -13.49 32.5))))
                               (COL (MAKE-MARRAY 'SINGLE-FLOAT :DIMENSIONS '3)))
                           (CL-ARRAY (COLUMN COL M1 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(8.24d0 34.12d0 -13.49d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((M1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '((-34.5d0 8.24d0 3.29d0)
                                               (-8.93d0 34.12d0 -6.15d0)
                                               (49.27d0 -13.49d0 32.5d0))))
                               (COL (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '3)))
                           (CL-ARRAY (COLUMN COL M1 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(#C(3.29 -8.93) #C(-6.15 49.27) #C(32.5 42.73)))
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
                               (COL
                                (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
                                             :DIMENSIONS '3)))
                           (CL-ARRAY (COLUMN COL M1 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(#C(3.29d0 -8.93d0) #C(-6.15d0 49.27d0)
                           #C(32.5d0 42.73d0)))
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
                               (COL
                                (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :DIMENSIONS '3)))
                           (CL-ARRAY (COLUMN COL M1 1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-68 52 -5))
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
                                                                      123))))
                                                                 (COL
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :DIMENSIONS
                                                                   '3)))
                                                             (CL-ARRAY
                                                              (COLUMN COL M1
                                                                      1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 163 215))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '((67 44
                                                                      189)
                                                                     (116 163
                                                                      140)
                                                                     (161 215
                                                                      98))))
                                                                 (COL
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :DIMENSIONS
                                                                   '3)))
                                                             (CL-ARRAY
                                                              (COLUMN COL M1
                                                                      1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-68 52 -5))
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
                                                                      123))))
                                                                 (COL
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :DIMENSIONS
                                                                   '3)))
                                                             (CL-ARRAY
                                                              (COLUMN COL M1
                                                                      1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 163 215))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '((67 44
                                                                      189)
                                                                     (116 163
                                                                      140)
                                                                     (161 215
                                                                      98))))
                                                                 (COL
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :DIMENSIONS
                                                                   '3)))
                                                             (CL-ARRAY
                                                              (COLUMN COL M1
                                                                      1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-68 52 -5))
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
                                                                      123))))
                                                                 (COL
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :DIMENSIONS
                                                                   '3)))
                                                             (CL-ARRAY
                                                              (COLUMN COL M1
                                                                      1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 163 215))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '((67 44
                                                                      189)
                                                                     (116 163
                                                                      140)
                                                                     (161 215
                                                                      98))))
                                                                 (COL
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :DIMENSIONS
                                                                   '3)))
                                                             (CL-ARRAY
                                                              (COLUMN COL M1
                                                                      1)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-68 52 -5))
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
                                                                      123))))
                                                                 (COL
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :DIMENSIONS
                                                                   '3)))
                                                             (CL-ARRAY
                                                              (COLUMN COL M1
                                                                      1)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 163 215))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((M1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '((67 44
                                                                      189)
                                                                     (116 163
                                                                      140)
                                                                     (161 215
                                                                      98))))
                                                                 (COL
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :DIMENSIONS
                                                                   '3)))
                                                             (CL-ARRAY
                                                              (COLUMN COL M1
                                                                      1))))))

