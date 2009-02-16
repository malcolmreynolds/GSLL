;; Regression test NUMERICAL-DIFFERENTIATION for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST NUMERICAL-DIFFERENTIATION
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.718281828441488d0 4.123659913167868d-10)
                        (MULTIPLE-VALUE-LIST
                         (CENTRAL-DERIVATIVE 'DERIV-F1 1.0d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.7182817825298398d0 9.540320743340577d-7)
                        (MULTIPLE-VALUE-LIST
                         (FORWARD-DERIVATIVE 'DERIV-F1 1.0d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.7182818293827378d0 9.086969640080628d-7)
                        (MULTIPLE-VALUE-LIST
                         (BACKWARD-DERIVATIVE 'DERIV-F1 1.0d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.474341649024533d0 3.3922603135575853d-11)
                        (MULTIPLE-VALUE-LIST
                         (CENTRAL-DERIVATIVE 'DERIV-F2 0.1d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.47434164943235285d0 9.102230432046565d-8)
                        (MULTIPLE-VALUE-LIST
                         (FORWARD-DERIVATIVE 'DERIV-F2 0.1d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4743416462049176d0 8.787851752748856d-8)
                        (MULTIPLE-VALUE-LIST
                         (BACKWARD-DERIVATIVE 'DERIV-F2 0.1d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.9941773945923913d0 6.730277498876722d-10)
                        (MULTIPLE-VALUE-LIST
                         (CENTRAL-DERIVATIVE 'DERIV-F3 0.45d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.994177385772526d0 1.6952086150997503d-6)
                        (MULTIPLE-VALUE-LIST
                         (FORWARD-DERIVATIVE 'DERIV-F3 0.45d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.9941775182635064d0 1.568063267060477d-6)
                        (MULTIPLE-VALUE-LIST
                         (BACKWARD-DERIVATIVE 'DERIV-F3 0.45d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.7788007830653751d0 1.7227267499194736d-10)
                        (MULTIPLE-VALUE-LIST
                         (CENTRAL-DERIVATIVE 'DERIV-F4 0.5d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.7788007884386108d0 2.530006603059119d-7)
                        (MULTIPLE-VALUE-LIST
                         (FORWARD-DERIVATIVE 'DERIV-F4 0.5d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.7788007743421791d0 2.503617930568489d-7)
                        (MULTIPLE-VALUE-LIST
                         (BACKWARD-DERIVATIVE 'DERIV-F4 0.5d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0d0 6.66133814775094d-20)
                        (MULTIPLE-VALUE-LIST
                         (CENTRAL-DERIVATIVE 'DERIV-F5 0.0d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -2.6337311035821125d-26 1.606762230663824d-11)
                        (MULTIPLE-VALUE-LIST
                         (FORWARD-DERIVATIVE 'DERIV-F5 0.0d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 2.6337311035821125d-26 1.606762230663824d-11)
                        (MULTIPLE-VALUE-LIST
                         (BACKWARD-DERIVATIVE 'DERIV-F5 0.0d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.009999999999871223d0 2.6645352591887814d-12)
                        (MULTIPLE-VALUE-LIST
                         (CENTRAL-DERIVATIVE 'DERIV-F6 10.0d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.010000000266238856d0 4.9950606958271945d-9)
                        (MULTIPLE-VALUE-LIST
                         (FORWARD-DERIVATIVE 'DERIV-F6 10.0d0 1.d-4)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.010000000076196142d0 4.641550044799095d-9)
                        (MULTIPLE-VALUE-LIST
                         (BACKWARD-DERIVATIVE 'DERIV-F6 10.0d0 1.d-4))))

