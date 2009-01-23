;; Regression test MINIMIZATION-ONE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MINIMIZATION-ONE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6 3.141474321994987d0 3.1415930343642042d0
                              3.141592654724622d0 1.134828675475319d-9
                              1.1871236921701112d-4)
                        (MULTIPLE-VALUE-LIST
                         (MINIMIZATION-ONE-EXAMPLE *BRENT-FMINIMIZER* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 24 3.1413247152275243d0 3.1419619412229034d0
                              3.1415652995716155d0 -2.7354018177661032d-5
                              6.37225995379076d-4)
                        (MULTIPLE-VALUE-LIST
                         (MINIMIZATION-ONE-EXAMPLE *GOLDEN-SECTION-FMINIMIZER*
                                                   NIL))))

