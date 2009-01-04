;; Regression test VECTOR-MULT-SCALAR for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-MULT-SCALAR
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-47.955 11.4536 4.5731))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29))))
                           (CL-ARRAY (ELT* V1 1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(-47.955d0 11.4536d0 4.5731d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0))))
                           (CL-ARRAY (ELT* V1 1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-88 -94 98))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (CL-ARRAY
                                                              (ELT* V1
                                                                   1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(93 61 6))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (CL-ARRAY
                                                              (ELT* V1
                                                                   1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-88 -94 98))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (CL-ARRAY
                                                              (ELT* V1
                                                                   1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(93 61 262))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (CL-ARRAY
                                                              (ELT* V1
                                                                   1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-88 -94 98))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (CL-ARRAY
                                                              (ELT* V1
                                                                   1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(93 61 262))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (CL-ARRAY
                                                              (ELT* V1
                                                                   1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-88 -94 98))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (CL-ARRAY
                                                              (ELT* V1
                                                                   1.39d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(93 61 262))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (CL-ARRAY
                                                              (ELT* V1
                                                                   1.39d0))))))

