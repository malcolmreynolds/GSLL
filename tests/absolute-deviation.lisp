;; Regression test ABSOLUTE-DEVIATION for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ABSOLUTE-DEVIATION
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 6.18d0 6.647777777777779d0 6.18d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((VEC
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-3.21d0 1.0d0 12.8d0)))
                               (WEIGHTS
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(3.0d0 1.0d0 2.0d0))))
                           (LET ((MEAN (MEAN VEC)))
                             (LIST (ABSOLUTE-DEVIATION VEC)
                                   (WEIGHTED-ABSOLUTE-DEVIATION VEC WEIGHTS)
                                   (ABSOLUTE-DEVIATION VEC MEAN)))))))

