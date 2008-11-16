;; Regression test SPHERICAL-VECTOR for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SPHERICAL-VECTOR
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST -0.617745613497854d0 -0.7863779988047479d0
                               0.993748310886084d0 0.1116436053298841d0
                               -0.9458104280982743d0 0.3247193158722761d0
                               0.45726622946182216d0 0.8893298574734622d0
                               -0.46325616159849964d0 -0.8862244234622655d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 4 APPEND
                                     (MULTIPLE-VALUE-LIST
                                      (DIRECTION-2D RNG))))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.9999986835208556d0 -0.0016226387631051197d0
                               0.5203010106077766d0 0.8539829379797504d0
                               -0.2035120531038584d0 0.9790724407527016d0
                               0.9454753227485545d0 -0.3256937427607672d0
                               0.11500033916619544d0 0.9933654523848008d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 4 APPEND
                                     (MULTIPLE-VALUE-LIST
                                      (DIRECTION-2D-TRIG-METHOD RNG))))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST -0.09129925750445994d0 0.18782185357162273d0
                               0.977950610665004d0 -0.9051182961559773d0
                               -0.050683764485791594d0 -0.4221279734645046d0
                               0.13993766535985133d0 0.8385462620524484d0
                               -0.526552576872909d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 2 APPEND
                                     (MULTIPLE-VALUE-LIST
                                      (DIRECTION-3D RNG)))))))

