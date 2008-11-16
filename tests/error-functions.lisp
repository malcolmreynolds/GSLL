;; Regression test ERROR-FUNCTIONS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ERROR-FUNCTIONS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8427007929497149d0 7.789237746491556d-16)
                        (MULTIPLE-VALUE-LIST (ERF 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.1572992070502851d0 4.0468944536809554d-16)
                        (MULTIPLE-VALUE-LIST (ERFC 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.8496055099332485d0 3.394126565390616d-15)
                        (MULTIPLE-VALUE-LIST (LOG-ERFC 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.24197072451914334d0 1.611848817878303d-16)
                        (MULTIPLE-VALUE-LIST (ERF-Z 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.15865525393145707d0 2.832400331480832d-16)
                        (MULTIPLE-VALUE-LIST (ERF-Q 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.5251352761609807d0 5.532094155354489d-15)
                        (MULTIPLE-VALUE-LIST (HAZARD 1.0d0))))

