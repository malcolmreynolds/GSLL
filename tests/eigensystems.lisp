;; Regression test EIGENSYSTEMS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST EIGENSYSTEMS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(13.819660112501051d0 36.180339887498945d0 40.0d0)
                         #2A((0.8506508083520399d0 -0.5257311121191337d0 0.0d0)
                             (0.5257311121191337d0 0.8506508083520399d0 0.0d0)
                             (0.0d0 0.0d0 1.0d0)))
                        (MULTIPLE-VALUE-LIST
                         (EIGENVALUE-EIGENVECTORS-EXAMPLE))))

