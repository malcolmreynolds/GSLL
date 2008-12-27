;; Regression test VECTOR-M+ for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-M+
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-43.43 42.36 -2.8600001))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29)))
                               (V2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-8.93 34.12 -6.15))))
                           (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-43.43d0 42.36d0 -2.8600000000000003d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0)))
                               (V2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-8.93d0 34.12d0 -6.15d0))))
                           (CL-ARRAY (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(101 -16 61))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(-91 52
                                                                     -10))))
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 207 73))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(116 163
                                                                     140))))
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-155 -16 61))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(-91 52
                                                                     -10))))
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 207 329))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(116 163
                                                                     140))))
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-155 -16 61))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(-91 52
                                                                     -10))))
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 207 329))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(116 163
                                                                     140))))
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-155 -16 61))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(-91 52
                                                                     -10))))
                                                             (CL-ARRAY
                                                              (M+ V1 V2)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(183 207 329))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(116 163
                                                                     140))))
                                                             (CL-ARRAY
                                                              (M+ V1 V2))))))

