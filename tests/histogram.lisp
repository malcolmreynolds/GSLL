;; Regression test HISTOGRAM for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST HISTOGRAM
                       (LISP-UNIT:ASSERT-ERROR 'input-domain
                                               (LETM ((HISTO (HISTOGRAM 10)))
                                                     (SET-RANGES-UNIFORM HISTO
                                                                         0.0d0
                                                                         10.0d0)
                                                     (INCREMENT HISTO -2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((HISTO
                                                              (HISTOGRAM 10)))
                                                            (SET-RANGES-UNIFORM
                                                             HISTO 0.0d0
                                                             10.0d0)
                                                            (INCREMENT HISTO
                                                                       2.7d0)
                                                            (INCREMENT HISTO
                                                                       6.9d0
                                                                       2.0d0)
                                                            (MAREF HISTO 1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((HISTO
                                                              (HISTOGRAM 10)))
                                                            (SET-RANGES-UNIFORM
                                                             HISTO 0.0d0
                                                             10.0d0)
                                                            (INCREMENT HISTO
                                                                       2.7d0)
                                                            (INCREMENT HISTO
                                                                       6.9d0
                                                                       2.0d0)
                                                            (MAREF HISTO 2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 2.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((HISTO
                                                              (HISTOGRAM 10)))
                                                            (SET-RANGES-UNIFORM
                                                             HISTO 0.0d0
                                                             10.0d0)
                                                            (INCREMENT HISTO
                                                                       2.7d0)
                                                            (INCREMENT HISTO
                                                                       6.9d0
                                                                       2.0d0)
                                                            (MAREF HISTO 6))))
                       (LISP-UNIT:ASSERT-ERROR 'input-domain
                                               (LETM ((HISTO (HISTOGRAM 10)))
                                                     (SET-RANGES-UNIFORM HISTO
                                                                         0.0d0
                                                                         10.0d0)
                                                     (INCREMENT HISTO 2.7d0)
                                                     (INCREMENT HISTO 6.9d0
                                                                2.0d0)
                                                     (MAREF HISTO 16)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.0d0 10.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((HISTO
                                                              (HISTOGRAM 10)))
                                                            (SET-RANGES-UNIFORM
                                                             HISTO 0.0d0
                                                             10.0d0)
                                                            (INCREMENT HISTO
                                                                       2.7d0)
                                                            (INCREMENT HISTO
                                                                       6.9d0
                                                                       2.0d0)
                                                            (VALUES
                                                             (MIN-RANGE HISTO)
                                                             (MAX-RANGE
                                                              HISTO)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 10)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((HISTO
                                                              (HISTOGRAM 10)))
                                                            (SET-RANGES-UNIFORM
                                                             HISTO 0.0d0
                                                             10.0d0)
                                                            (INCREMENT HISTO
                                                                       2.7d0)
                                                            (INCREMENT HISTO
                                                                       6.9d0
                                                                       2.0d0)
                                                            (BINS HISTO))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 5)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((HISTO
                                                              (HISTOGRAM 10)))
                                                            (SET-RANGES-UNIFORM
                                                             HISTO 0.0d0
                                                             10.0d0)
                                                            (INCREMENT HISTO
                                                                       2.7d0)
                                                            (INCREMENT HISTO
                                                                       6.9d0
                                                                       2.0d0)
                                                            (HISTOGRAM-FIND
                                                             HISTO 5.5d0)))))

