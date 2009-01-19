;; Regression test ODE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ODE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0d0 -1.4568622636249005d0
                              -11.547385179410822d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 NIL))))

