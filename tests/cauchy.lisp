;; Regression test CAUCHY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST CAUCHY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST -0.00811319915595434d0 5.617196410586812d0
                               12.292369828923075d0 -1.6741088357812182d0
                               8.909104486260928d0 211.6765861544609d0
                               -1.3439049184367153d0 -10.364363282910663d0
                               -79.0709314248171d0 -10.652071087998578d0
                               -9.393948243493877d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 10 COLLECT
                                     (CAUCHY RNG 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.03183098861837907d0)
                        (MULTIPLE-VALUE-LIST (CAUCHY-PDF 0.0d0 10.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6475836176504333d0)
                        (MULTIPLE-VALUE-LIST (CAUCHY-P 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.35241638234956674d0)
                        (MULTIPLE-VALUE-LIST (CAUCHY-Q 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9999999999999998d0)
                        (MULTIPLE-VALUE-LIST
                         (CAUCHY-PINV 0.6475836176504333d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0000000000000002d0)
                        (MULTIPLE-VALUE-LIST
                         (CAUCHY-QINV 0.35241638234956674d0 2.0d0))))

