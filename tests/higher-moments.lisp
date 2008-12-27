;; Regression test HIGHER-MOMENTS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST HIGHER-MOMENTS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.2765118983985497d0 0.2765118983985497d0
                               -2.333333333333333d0 -2.333333333333333d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((VEC
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-3.21d0 1.0d0 12.8d0))))
                           (LET* ((MEAN (MEAN VEC))
                                  (SD (STANDARD-DEVIATION VEC MEAN)))
                             (LIST (SKEWNESS VEC) (SKEWNESS VEC MEAN SD)
                                   (KURTOSIS VEC) (KURTOSIS VEC MEAN SD)))))))

