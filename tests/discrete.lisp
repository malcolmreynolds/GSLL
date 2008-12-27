;; Regression test DISCRETE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST DISCRETE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 1 0 1 1 0 1 1 2 1 2 2))
                        (MULTIPLE-VALUE-LIST
                         (LET* ((PROBABILITIES
                                 (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                              '(0.25d0 0.5d0 0.25d0)))
                                (TABLE (MAKE-DISCRETE-RANDOM PROBABILITIES))
                                (RNG
                                 (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (DISCRETE RNG TABLE)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.5d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET* ((PROBABILITIES
                                                                   (MAKE-MARRAY
                                                                    'DOUBLE-FLOAT
                                                                    :INITIAL-CONTENTS
                                                                    '(0.25d0
                                                                      0.5d0
                                                                      0.25d0)))
                                                                  (TABLE
                                                                   (MAKE-DISCRETE-RANDOM
                                                                    PROBABILITIES)))
                                                             (DISCRETE-PDF 1
                                                                           TABLE)))))

