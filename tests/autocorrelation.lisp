;; Regression test AUTOCORRELATION for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST AUTOCORRELATION
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST -0.04646366834251103d0 -0.04646366834251103d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((VEC
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-3.21d0 1.0d0 12.8d0))))
                           (LET ((MEAN (MEAN VEC)))
                             (LIST (AUTOCORRELATION VEC)
                                   (AUTOCORRELATION VEC MEAN)))))))

