;; Regression test PARETO for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST PARETO
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 2.0005166356083666d0 12.276726596218747d0
                               7.076694965937239d0 2.111484074469764d0
                               8.633470811095883d0 4.123935696449152d0
                               2.0888231161547828d0 2.6870692498025632d0
                               3.703404287965457d0 2.7028744394290123d0
                               2.631773566385122d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (PARETO RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5116034405707658d0)
                        (MULTIPLE-VALUE-LIST (PARETO-PDF 1.5d0 1.3d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5168849835182453d0)
                        (MULTIPLE-VALUE-LIST (PARETO-P 3.5d0 1.3d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4831150164817547d0)
                        (MULTIPLE-VALUE-LIST (PARETO-Q 3.5d0 1.3d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 3.5000000000000004d0)
                        (MULTIPLE-VALUE-LIST
                         (PARETO-PINV 0.5168849835182453d0 1.3d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 3.5000000000000004d0)
                        (MULTIPLE-VALUE-LIST
                         (PARETO-QINV 0.4831150164817547d0 1.3d0 2.0d0))))

