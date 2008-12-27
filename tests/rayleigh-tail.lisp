;; Regression test RAYLEIGH-TAIL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST RAYLEIGH-TAIL
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 1.0255032370386696d0 19.0764679267351d0
                               15.928966102255199d0 3.4422048899106383d0
                               17.131838333441106d0 12.071957529361999d0
                               3.112992916690818d0 7.749889301203328d0
                               11.145450138119857d0 7.825198187316554d0
                               7.476774681552917d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (RAYLEIGH-TAIL RNG 1.0d0 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.10224317624874313d0)
                        (MULTIPLE-VALUE-LIST
                         (RAYLEIGH-TAIL-PDF 0.25d0 -2.0d0 2.0d0))))

