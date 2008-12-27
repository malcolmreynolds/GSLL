;; Regression test RAYLEIGH for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST RAYLEIGH
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.22728151965522753d0 19.05023959323748d0
                               15.897545756713367d0 3.2937477899992147d0
                               17.102628005168157d0 12.030467929000928d0
                               2.9480035446666624d0 7.6851014424603274d0
                               11.100498132125239d0 7.76103902005281d0
                               7.409599155063027d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (RAYLEIGH RNG 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4412484512922977d0)
                        (MULTIPLE-VALUE-LIST (RAYLEIGH-PDF 0.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.1175030974154046d0)
                        (MULTIPLE-VALUE-LIST (RAYLEIGH-P 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.8824969025845955d0)
                        (MULTIPLE-VALUE-LIST (RAYLEIGH-Q 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.0000000000000002d0)
                        (MULTIPLE-VALUE-LIST
                         (RAYLEIGH-PINV 0.1175030974154046d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9999999999999998d0)
                        (MULTIPLE-VALUE-LIST
                         (RAYLEIGH-QINV 0.8824969025845955d0 2.0d0))))

