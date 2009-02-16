;; Regression test EXPONENTIAL-POWER for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST EXPONENTIAL-POWER
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.09469475592777954d0 -0.06229680875327071d0
                               1.183985538537803d0 0.5187626019237904d0
                               0.7053564314063956d0 -0.9033303844569821d0
                               -1.6947336289940842d0 -0.4803236108055401d0
                               -0.027641736349912214d0 0.6318391856046153d0
                               -0.012478875227423025d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (EXPONENTIAL-POWER RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5641895835477557d0)
                        (MULTIPLE-VALUE-LIST
                         (EXPONENTIAL-POWER-PDF 0.0d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9213503964748571d0)
                        (MULTIPLE-VALUE-LIST
                         (EXPONENTIAL-POWER-P 1.0d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.07864960352514248d0)
                        (MULTIPLE-VALUE-LIST
                         (EXPONENTIAL-POWER-Q 1.0d0 1.0d0 2.0d0))))

