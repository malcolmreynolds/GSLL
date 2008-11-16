;; Regression test AIRY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST AIRY
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.01572592338047048d0 2.1573014423586447d-17)
                        (MULTIPLE-VALUE-LIST (AIRY-AI 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 6.48166073846058d0 8.77836191730236d-15)
                        (MULTIPLE-VALUE-LIST (AIRY-BI 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.21932220512871203d0 5.966864198455728d-17)
                        (MULTIPLE-VALUE-LIST (AIRY-AI-SCALED 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.4647504801960925d0 1.1831869152362144d-16)
                        (MULTIPLE-VALUE-LIST (AIRY-BI-SCALED 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.02625088103590322d0 4.306971270221159d-17)
                        (MULTIPLE-VALUE-LIST (AIRY-AI-DERIV 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 9.421423317334305d0 1.5213125884867257d-14)
                        (MULTIPLE-VALUE-LIST (AIRY-BI-DERIV 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -0.36610893847516224d0 1.167515239400716d-16)
                        (MULTIPLE-VALUE-LIST (AIRY-AI-DERIV-SCALED 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6755384441644995d0 1.978922049880242d-16)
                        (MULTIPLE-VALUE-LIST (AIRY-BI-DERIV-SCALED 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -2.338107410459767d0 5.19164136227827d-16)
                        (MULTIPLE-VALUE-LIST (AIRY-ZERO-AI 1)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.173713222709128d0 2.606166888317336d-16)
                        (MULTIPLE-VALUE-LIST (AIRY-ZERO-BI 1)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -1.018792971647471d0 2.2621748288986134d-16)
                        (MULTIPLE-VALUE-LIST (AIRY-ZERO-AI-DERIV 1)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST -2.294439682614123d0 5.094679528503672d-16)
                        (MULTIPLE-VALUE-LIST (AIRY-ZERO-BI-DERIV 1))))

