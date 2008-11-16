;; Regression test PSI for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST PSI
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.2561176684318005d0 2.789141514262906d-16)
                        (MULTIPLE-VALUE-LIST (PSI 4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.2561176684318005d0 2.846229783626858d-16)
                        (MULTIPLE-VALUE-LIST (PSI 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.7145915153739806d0 1.925418559790263d-14)
                        (MULTIPLE-VALUE-LIST (PSI-1+IY 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.2838229557371153d0 6.302135607530242d-17)
                        (MULTIPLE-VALUE-LIST (PSI-1 4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.28382295573711525d0 1.764597970108467d-15)
                        (MULTIPLE-VALUE-LIST (PSI-1 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.0800397322451145d0 5.222647053360405d-16)
                        (MULTIPLE-VALUE-LIST (PSI-N 2 4.0d0))))

