;; Regression test ODE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST ODE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 495 1.0d0 -1.4568657425802234d0
                              -11.547345633897558d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-RK2* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 40 1.0d0 -1.4568569264026898d0
                              -11.547449151779395d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-RK4* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 35 1.0d0 -1.456874342553472d0
                              -11.547250693698407d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-RKF45* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 27 1.0d0 -1.4568588825970334d0
                              -11.547432342449643d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-RKCK* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 16 1.0d0 -1.4568622636249005d0
                              -11.547385179410822d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-RK8PD* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 359 1.0d0 -1.4569507371916566d0
                              -11.546406519788615d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-RK2IMP* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 40 1.0d0 -1.4568644436344684d0
                              -11.547361181933427d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-RK4IMP* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 9 1.0d0 -1.4568620806209944d0
                              -11.547387321938151d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-BSIMP* NIL)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 11176 1.0d0 -1.459959741392502d0
                              -11.510866371379606d0)
                        (MULTIPLE-VALUE-LIST
                         (LET ((*MAX-ITER* 12000))
                           (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-GEAR1*
                                                NIL))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 52 1.0d0 -1.4568645170220367d0
                              -11.54736019525012d0)
                        (MULTIPLE-VALUE-LIST
                         (INTEGRATE-VANDERPOL 1.0d0 1.d-4 *STEP-GEAR2* NIL))))

