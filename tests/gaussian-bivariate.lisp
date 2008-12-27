;; Regression test GAUSSIAN-BIVARIATE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST GAUSSIAN-BIVARIATE
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST -0.06509716124488897d0 -1.5733207749096374d0
                               0.27942740172325414d0 1.2021528358889673d0
                               -0.6041530626907894d0 0.07582702719413444d0
                               -0.5446229412165632d0 -0.6592026841613081d0
                               -0.11029516610819164d0 0.17931840412143885d0
                               2.1025104980291696d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (BIVARIATE-GAUSSIAN RNG 1.0d0 0.75d0
                                                     0.25d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.5548265557970462d0)
                        (MULTIPLE-VALUE-LIST
                         (BIVARIATE-GAUSSIAN-PDF 0.25d0 0.5d0 0.25d0 0.4d0
                                                 0.2d0))))

