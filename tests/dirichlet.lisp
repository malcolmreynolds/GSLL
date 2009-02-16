;; Regression test DIRICHLET for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST DIRICHLET
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.12448073544131681d0 0.19182353706734917d0
                           0.46054388544826397d0 0.22315184204307006d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0))
                               (ALPHA
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(1.0d0 2.0d0 3.0d0 4.0d0)))
                               (THETA
                                (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS 4)))
                           (DIRICHLET RNG ALPHA THETA)
                           (CL-ARRAY THETA))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1080.0000000000025d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((ALPHA
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(1.0d0 2.0d0 3.0d0 4.0d0)))
                               (THETA
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(1.0d0 2.0d0 3.0d0 4.0d0))))
                           (DIRICHLET-PDF ALPHA THETA))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6.984716320118268d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((ALPHA
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(1.0d0 2.0d0 3.0d0 4.0d0)))
                               (THETA
                                (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
                                             '(1.0d0 2.0d0 3.0d0 4.0d0))))
                           (DIRICHLET-LOG-PDF ALPHA THETA)))))

