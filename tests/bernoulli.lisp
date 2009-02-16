;; Regression test BERNOULLI for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST BERNOULLI
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 0 1 1 0 1 1 0 0 0 0 0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (BERNOULLI RNG 0.5d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.5d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (BERNOULLI-PDF 0
                                                                          0.5d0))))

