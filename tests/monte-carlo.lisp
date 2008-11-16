;; Regression test MONTE-CARLO for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MONTE-CARLO
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.4122087033540667d0 0.013435861456267064d0)
                        (MULTIPLE-VALUE-LIST (RANDOM-WALK-PLAIN-EXAMPLE)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.3895297058825096d0 0.0050106269732269415d0)
                        (MULTIPLE-VALUE-LIST (RANDOM-WALK-MISER-EXAMPLE)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.3931632739551914d0 1.4981164744582692d-4)
                        (MULTIPLE-VALUE-LIST (RANDOM-WALK-VEGAS-EXAMPLE))))

