;; Regression test GAUSSIAN-TAIL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST GAUSSIAN-TAIL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 50.10837030381376d0 51.42695945309931d0
                               50.587160269982604d0 50.59875222444504d0
                               50.82830572864337d0 50.43343112125345d0
                               53.442286287287374d0 51.83761714183811d0
                               53.00107421429086d0 52.149774169929884d0
                               50.11572443504253d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (GAUSSIAN-TAIL RNG 50.0d0 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.18702270877331703d0)
                        (MULTIPLE-VALUE-LIST
                         (GAUSSIAN-TAIL-PDF 52.0d0 50.0d0 10.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 5.010837030381376d0 5.142695945309931d0
                               5.05871602699826d0 5.0598752224445045d0
                               5.082830572864337d0 5.043343112125345d0
                               5.344228628728738d0 5.183761714183811d0
                               5.300107421429086d0 5.214977416992989d0
                               5.011572443504253d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (UGAUSSIAN-TAIL RNG 5.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 1.8702270877331704d0)
                        (MULTIPLE-VALUE-LIST (UGAUSSIAN-TAIL-PDF 5.2d0 5.0d0))))

