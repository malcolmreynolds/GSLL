;; Regression test CORRELATION for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST CORRELATION
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6345226832058398d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29)))
                               (V2
                                (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
                                             '(-8.93 34.12 -6.15))))
                           (CORRELATION V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6345226817840662d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0)))
                               (V2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-8.93d0 34.12d0 -6.15d0))))
                           (CORRELATION V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.05125843466531291d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
                                             '(-64 -68 71)))
                               (V2
                                (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
                                             '(-91 52 -10))))
                           (CORRELATION V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.13540958697634814d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS '(67 44 189)))
                               (V2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 8)
                                             :INITIAL-CONTENTS '(116 163 140))))
                           (CORRELATION V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.05125843466531291d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS '(-64 -68 71)))
                               (V2
                                (MAKE-MARRAY '(SIGNED-BYTE 16)
                                             :INITIAL-CONTENTS '(-91 52 -10))))
                           (CORRELATION V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.13540958697634814d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS '(67 44 189)))
                               (V2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 16)
                                             :INITIAL-CONTENTS '(116 163 140))))
                           (CORRELATION V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.05125843466531291d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS '(-64 -68 71)))
                               (V2
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS '(-91 52 -10))))
                           (CORRELATION V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.13540958697634814d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS '(67 44 189)))
                               (V2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 32)
                                             :INITIAL-CONTENTS '(116 163 140))))
                           (CORRELATION V1 V2))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.05125843466531291d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS '(-64 -68 71)))
                               (V2
                                (MAKE-MARRAY '(SIGNED-BYTE 64)
                                             :INITIAL-CONTENTS '(-91 52 -10))))
                           (CORRELATION V1 V2))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.13540958697634814d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS '(67 44 189)))
                               (V2
                                (MAKE-MARRAY '(UNSIGNED-BYTE 64)
                                             :INITIAL-CONTENTS '(116 163 140))))
                           (CORRELATION V1 V2)))))
