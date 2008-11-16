;; Regression test DIRICHLET for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST DIRICHLET
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.12448073544131681d0 0.19182353706734917d0
                           0.46054388544826397d0 0.22315184204307006d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0))
                           (ALPHA
                            (VECTOR-DOUBLE-FLOAT (A 1.0d0 2.0d0 3.0d0 4.0d0)))
                           (THETA (VECTOR-DOUBLE-FLOAT 4)))
                          (DIRICHLET RNG ALPHA THETA) (CL-ARRAY THETA))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.8800000000000043d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((ALPHA
                            (VECTOR-DOUBLE-FLOAT (A 1.0d0 2.0d0 3.0d0 4.0d0)))
                           (THETA
                            (VECTOR-DOUBLE-FLOAT (A 0.1d0 0.3d0 0.4d0 0.2d0))))
                          (DIRICHLET-PDF ALPHA THETA))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.057790294147856d0)
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((ALPHA
                            (VECTOR-DOUBLE-FLOAT (A 1.0d0 2.0d0 3.0d0 4.0d0)))
                           (THETA
                            (VECTOR-DOUBLE-FLOAT (A 0.1d0 0.3d0 0.4d0 0.2d0))))
                          (DIRICHLET-LOG-PDF ALPHA THETA)))))

