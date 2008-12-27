;; Regression test VECTOR-SET-ALL-M+ for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-SET-ALL-M+
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-43.43 -0.69000053 -5.6400003))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1 (MAKE-MARRAY 'SINGLE-FLOAT :DIMENSIONS '3))
                               (V2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29))))
                           (SET-ALL V1 -8.93)
                           (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-43.43d0 -0.6899999999999995d0 -5.64d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1 (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '3))
                               (V2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0))))
                           (SET-ALL V1 -8.93d0)
                           (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(101 97 -20))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :DIMENSIONS
                                                                   '3))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (SET-ALL V1 -91)
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 160 49))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :DIMENSIONS
                                                                   '3))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (SET-ALL V1 116)
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-155 -159 -20))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 16) :DIMENSIONS '3))
                               (V2
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS '(-64 -68 71))))
                           (SET-ALL V1 -91)
                           (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 160 305))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :DIMENSIONS
                                                                   '3))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (SET-ALL V1 116)
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-155 -159 -20))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 32) :DIMENSIONS '3))
                               (V2
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS '(-64 -68 71))))
                           (SET-ALL V1 -91)
                           (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 160 305))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :DIMENSIONS
                                                                   '3))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (SET-ALL V1 116)
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-155 -159 -20))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 64) :DIMENSIONS '3))
                               (V2
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS '(-64 -68 71))))
                           (SET-ALL V1 -91)
                           (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 160 305))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :DIMENSIONS
                                                                   '3))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (SET-ALL V1 116)
                                                             (CL-ARRAY
                                                              (M+ V1 V2))))))

