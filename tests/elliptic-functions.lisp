;; Regression test ELLIPTIC-FUNCTIONS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ELLIPTIC-FUNCTIONS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.19762082367187703d0 0.9802785369736752d0
                              0.9840560289645665d0 0.0d0 0.0d0 0.0d0)
                        (MULTIPLE-VALUE-LIST
                         (JACOBIAN-ELLIPTIC-FUNCTIONS 0.2d0 0.81d0)))
                       (LISP-UNIT:ASSERT-ERROR 'INPUT-DOMAIN
                                               (JACOBIAN-ELLIPTIC-FUNCTIONS
                                                0.61802d0 1.5d0)))

