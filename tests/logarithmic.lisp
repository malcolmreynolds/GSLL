;; Regression test LOGARITHMIC for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LOGARITHMIC
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 1 3 1 4 1 1 2 1 1 5 2))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (LOGARITHMIC RNG 0.9d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.15660921511769743d0)
                        (MULTIPLE-VALUE-LIST (LOGARITHMIC-PDF 2 0.4d0))))

