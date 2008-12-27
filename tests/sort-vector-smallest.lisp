;; Regression test SORT-VECTOR-SMALLEST for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SORT-VECTOR-SMALLEST
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-34.5 -13.49 -8.93))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29 -8.93 34.12
                                               -6.15 49.27 -13.49)))
                               (V2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(32.5 42.73 -17.24))))
                           (CL-ARRAY (SORT-VECTOR-SMALLEST V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-34.5d0 -13.49d0 -8.93d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0 -8.93d0
                                               34.12d0 -6.15d0 49.27d0
                                               -13.49d0)))
                               (V2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(32.5d0 42.73d0 -17.24d0))))
                           (CL-ARRAY (SORT-VECTOR-SMALLEST V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-91 -68 -64))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68 71
                                                                     -91 52 -10
                                                                     73 -5)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(123 32
                                                                     28))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-SMALLEST
                                                               V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 67 116))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44 189
                                                                     116 163
                                                                     140 161
                                                                     215)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(98 28
                                                                     10))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-SMALLEST
                                                               V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-91 -68 -64))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68 71
                                                                     -91 52 -10
                                                                     73 -5)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(123 32
                                                                     28))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-SMALLEST
                                                               V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 67 116))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44 189
                                                                     116 163
                                                                     140 161
                                                                     215)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(98 28
                                                                     10))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-SMALLEST
                                                               V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-91 -68 -64))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68 71
                                                                     -91 52 -10
                                                                     73 -5)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(123 32
                                                                     28))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-SMALLEST
                                                               V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 67 116))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44 189
                                                                     116 163
                                                                     140 161
                                                                     215)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(98 28
                                                                     10))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-SMALLEST
                                                               V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-91 -68 -64))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68 71
                                                                     -91 52 -10
                                                                     73 -5)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(123 32
                                                                     28))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-SMALLEST
                                                               V2 V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(44 67 116))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44 189
                                                                     116 163
                                                                     140 161
                                                                     215)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(98 28
                                                                     10))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-SMALLEST
                                                               V2 V1))))))

