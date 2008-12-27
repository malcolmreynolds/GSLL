;; Regression test MEDIAN-PERCENTILE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MEDIAN-PERCENTILE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((VEC
                                                                  (MAKE-MARRAY
                                                                   'DOUBLE-FLOAT
                                                                   :INITIAL-CONTENTS
                                                                   '(-3.21d0
                                                                     1.0d0
                                                                     12.8d0))))
                                                             (MEDIAN VEC))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.85d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((VEC
                                                                  (MAKE-MARRAY
                                                                   'DOUBLE-FLOAT
                                                                   :INITIAL-CONTENTS
                                                                   '(-18.0d0
                                                                     -12.0d0
                                                                     -3.21d0
                                                                     0.5d0
                                                                     1.0d0
                                                                     2.7d0
                                                                     12.8d0))))
                                                             (QUANTILE VEC
                                                                       0.75d0)))))

