;; Regression test COVARIANCE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST COVARIANCE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST -0.2929999999999998d0 -0.2929999999999998d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((VEC1
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(-3.21d0 1.0d0 12.8d0)))
                               (VEC2
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(1.15d0 -1.0d0 0.5d0))))
                           (LET ((MEAN1 (MEAN VEC1)) (MEAN2 (MEAN VEC2)))
                             (LIST (COVARIANCE VEC1 VEC2)
                                   (COVARIANCE VEC1 VEC2 MEAN1 MEAN2)))))))

