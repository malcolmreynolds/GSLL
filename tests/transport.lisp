;; Regression test TRANSPORT for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST TRANSPORT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.806666404563118d0 2.2867923780257255d-15)
                        (MULTIPLE-VALUE-LIST (TRANSPORT-2 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 4.579217437229157d0 3.242324689309112d-15)
                        (MULTIPLE-VALUE-LIST (TRANSPORT-3 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 10.731932392998623d0 1.0925209116254758d-14)
                        (MULTIPLE-VALUE-LIST (TRANSPORT-4 4.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 29.488339015245842d0 3.204450601879883d-14)
                        (MULTIPLE-VALUE-LIST (TRANSPORT-5 4.0d0))))

