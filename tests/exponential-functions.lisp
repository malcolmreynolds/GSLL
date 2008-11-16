;; Regression test EXPONENTIAL-FUNCTIONS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST EXPONENTIAL-FUNCTIONS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 20.085536923187668d0 8.91977022163267d-15)
                        (MULTIPLE-VALUE-LIST (GSL-EXP 3.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 3.8811801942838993d0 868 3.448904081776264d-12)
                        (MULTIPLE-VALUE-LIST (EXP-SCALED 2000.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 3.6535299896840335d44 8.355840218353793d30)
                        (MULTIPLE-VALUE-LIST (EXP-MULT 101.0d0 5.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0908344123363103d0 243 5.42628544911082d-13)
                        (MULTIPLE-VALUE-LIST
                         (EXP-MULT-SCALED 555.0d0 101.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0000500016667085d-4 4.441114150507224d-20)
                        (MULTIPLE-VALUE-LIST (EXPM1 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0000500016667084d0 4.4411141505072235d-16)
                        (MULTIPLE-VALUE-LIST (EXPREL 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0003334166833362d0 4.442372766015162d-16)
                        (MULTIPLE-VALUE-LIST (EXPREL-2 0.001d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0002500500083344d0 2.665201526164121d-15)
                        (MULTIPLE-VALUE-LIST (EXPREL-N 3 0.001d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 20.085536923187668d0 0.04017108054156605d0)
                        (MULTIPLE-VALUE-LIST (EXP-ERR 3.0d0 0.001d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 461.9673492333164d0 0.4820528861567092d0)
                        (MULTIPLE-VALUE-LIST
                         (EXP-MULT-ERR 3.0d0 0.001d0 23.0d0 0.001d0))))

