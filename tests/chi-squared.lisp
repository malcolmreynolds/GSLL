;; Regression test CHI-SQUARED for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST CHI-SQUARED
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 13.043328884186328d0 11.427227829236712d0
                               16.55811815484942d0 7.128795406995407d0
                               5.120266499239882d0 10.464572605669142d0
                               5.8126929867006405d0 8.784940866479005d0
                               7.559275305609187d0 8.35181083950897d0
                               4.140798004825149d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (CHI-SQUARED RNG 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.43939128946772227d0)
                        (MULTIPLE-VALUE-LIST (CHI-SQUARED-PDF 0.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5204998778130462d0)
                        (MULTIPLE-VALUE-LIST (CHI-SQUARED-P 0.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4795001221869538d0)
                        (MULTIPLE-VALUE-LIST (CHI-SQUARED-Q 0.5d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5000000000000003d0)
                        (MULTIPLE-VALUE-LIST
                         (CHI-SQUARED-PINV 0.5204998778130463d0 1.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5000000000000003d0)
                        (MULTIPLE-VALUE-LIST
                         (CHI-SQUARED-QINV 0.4795001221869537d0 1.0d0))))

