;; Regression test LEVY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LEVY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 2.6941098332360465d0 -0.29395438644676647d0
                               -1.2703401352272083d0 1.0771538640113538d0
                               0.13771218406916103d0 0.9419728438107844d0
                               -0.5107134674789159d0 0.1648207853689268d0
                               -0.14857899041035147d0 -1.9074885744364487d0
                               -2.086195213997167d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 10 COLLECT
                                     (LEVY RNG 1.0d0 2.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 2.6941098332360465d0 -0.2939543864467665d0
                               -1.2703401352272083d0 1.0771538640113538d0
                               0.13771218406916097d0 0.9419728438107844d0
                               -0.510713467478916d0 0.1648207853689266d0
                               -0.14857899041035158d0 -1.907488574436449d0
                               -2.086195213997167d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 10 COLLECT
                                     (LEVY-SKEW RNG 1.0d0 2.0d0 1.0d0))))))

