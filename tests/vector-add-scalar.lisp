;; Regression test VECTOR-ADD-SCALAR for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-ADD-SCALAR
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-16.31 26.43 21.48))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29))))
                           (CL-ARRAY (M+C V1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-16.31d0 26.43d0 21.48d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0))))
                           (CL-ARRAY (M+C V1 18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-45 -49 89))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (CL-ARRAY
                                                              (M+C V1
                                                                   18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(85 62 207))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (CL-ARRAY
                                                              (M+C V1
                                                                   18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-45 -49 89))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (CL-ARRAY
                                                              (M+C V1
                                                                   18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(85 62 207))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (CL-ARRAY
                                                              (M+C V1
                                                                   18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-45 -49 89))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (CL-ARRAY
                                                              (M+C V1
                                                                   18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(85 62 207))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (CL-ARRAY
                                                              (M+C V1
                                                                   18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-45 -49 89))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (CL-ARRAY
                                                              (M+C V1
                                                                   18.19d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(85 62 207))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (CL-ARRAY
                                                              (M+C V1
                                                                   18.19d0))))))

