;; Regression test LINEAR-LEAST-SQUARES for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LINEAR-LEAST-SQUARES
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -106.59999999999998d0 0.05999999999999999d0
                              39601.99999999999d0 -19.9d0
                              0.009999999999999998d0 0.8d0)
                        (MULTIPLE-VALUE-LIST
                         (UNIVARIATE-LINEAR-LEAST-SQUARES-EXAMPLE NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.1824632487186013d0 0.1845715900137661d0
                              1.3031038153096723d0 16.006137036426168d0)
                        (MULTIPLE-VALUE-LIST
                         (MV-LINEAR-LEAST-SQUARES-EXAMPLE
                          (MV-LINEAR-LEAST-SQUARES-DATA) NIL))))

