;; Regression test QUASI-RANDOM-NUMBER-GENERATORS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST QUASI-RANDOM-NUMBER-GENERATORS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.5d0 0.5d0 0.75d0 0.25d0 0.25d0 0.75d0 0.375d0
                               0.375d0 0.875d0 0.875d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((GEN (QUASI-RANDOM-NUMBER-GENERATOR 2 *SOBOL*))
                           (VEC (VECTOR-DOUBLE-FLOAT 2)))
                          (LOOP REPEAT 5 DO (QRNG-GET GEN VEC) APPEND
                                (COERCE (CL-ARRAY VEC) 'LIST))))))

