;; Regression test GUMBEL1 for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST GUMBEL1
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 8.954596257487015d0 0.0973051899750762d0
                               0.45913506233088003d0 3.6074124224293223d0
                               0.31300027468174807d0 1.0165796949651174d0
                               3.8292081936610396d0 1.912897393181305d0
                               1.17748457894919d0 1.893232107970416d0
                               1.9859118616847695d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 10 COLLECT
                                     (GUMBEL1 RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.29625708964974956d0)
                        (MULTIPLE-VALUE-LIST (GUMBEL1-PDF 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.1637073598773166d0)
                        (MULTIPLE-VALUE-LIST (GUMBEL1-P 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8362926401226833d0)
                        (MULTIPLE-VALUE-LIST (GUMBEL1-Q 0.1d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.10000000000000007d0)
                        (MULTIPLE-VALUE-LIST
                         (GUMBEL1-PINV 0.1637073598773166d0 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.10000000000000028d0)
                        (MULTIPLE-VALUE-LIST
                         (GUMBEL1-QINV 0.8362926401226833d0 1.0d0 2.0d0))))

