;; Regression test VECTOR-MIN for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-MIN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -34.5)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   'SINGLE-FLOAT
                                                                   :INITIAL-CONTENTS
                                                                   '(-34.5 8.24
                                                                     3.29))))
                                                             (MMIN V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -34.5d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   'DOUBLE-FLOAT
                                                                   :INITIAL-CONTENTS
                                                                   '(-34.5d0
                                                                     8.24d0
                                                                     3.29d0))))
                                                             (MMIN V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -68)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (MMIN V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 44)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (MMIN V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -68)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (MMIN V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 44)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (MMIN V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -68)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (MMIN V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 44)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (MMIN V1))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -68)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68
                                                                     71))))
                                                             (MMIN V1))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 44)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44
                                                                     189))))
                                                             (MMIN V1)))))

