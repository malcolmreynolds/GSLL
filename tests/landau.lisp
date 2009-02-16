;; Regression test LANDAU for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST LANDAU
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 3880.037426254597d0 -0.6953200314545297d0
                               -0.02354364646600932d0 21.329209630030316d0
                               -0.3062224704714883d0 1.2424186669362394d0
                               26.146168479649152d0 4.337217640968217d0
                               1.6799546281085946d0 4.2475719218268395d0
                               4.681506208977819d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT (LANDAU RNG)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.17331968995860203d0)
                        (MULTIPLE-VALUE-LIST (LANDAU-PDF 0.25d0))))

