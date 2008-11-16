;; Regression test COULOMB for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST COULOMB
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.1641699972477976d0 1.8226531089715347d-16)
                        (MULTIPLE-VALUE-LIST (HYDROGENICR-1 1.0d0 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.07666468084146327d0 4.203054519905891d-16)
                        (MULTIPLE-VALUE-LIST (HYDROGENICR 3 1 1.0d0 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.06203505201137388d0 0.17709857491700906d0
                              3.6050175661599693d0 -5.8282618416439025d0 0.0d0
                              0.0d0 4.3793707124032857d-16
                              1.2502291640824433d-15 3.5328525523867457d-14
                              5.711592064490999d-14)
                        (MULTIPLE-VALUE-LIST
                         (COULOMB-WAVE-FG 0.0d0 1.0d0 2.0d0 0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.6617816138326813d0 0.3614128577450535d0
                           0.13267757609917497d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((ARR (VECTOR-DOUBLE-FLOAT 3)))
                               (COULOMB-WAVE-F-ARRAY 0.0d0 1.0d0 2.0d0 ARR)
                               (CL-ARRAY ARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.07161779967468254d0 0.1278101031568499d0
                              2.2510703464871114d0 -1.4245543587641651d0 0.0d0
                              0.0d0 8.269219937869759d-15
                              1.4757362807662922d-14 2.5991577338697564d-13
                              1.644835970887306d-13)
                        (MULTIPLE-VALUE-LIST
                         (COULOMB-WAVE-FG 1.0d0 2.0d0 2.5d0 1)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.035021584603913254d0 0.005752500614202263d0
                               7.116955601984411d-4 6.471726496134135d0
                               27.57457472159366d0 170.56037293106908d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM
                          ((FARR (VECTOR-DOUBLE-FLOAT 3))
                           (GARR (VECTOR-DOUBLE-FLOAT 3)))
                          (COULOMB-WAVE-FG-ARRAY 1.5d0 1.0d0 1.0d0 FARR GARR)
                          (APPEND (COERCE (CL-ARRAY FARR) 'LIST)
                                  (COERCE (CL-ARRAY GARR) 'LIST)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.33089080691634065d0 0.18070642887252675d0
                           0.06633878804958748d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((ARR (VECTOR-DOUBLE-FLOAT 3)))
                               (COULOMB-WAVE-SPHF-ARRAY 0.0d0 1.0d0 2.0d0 ARR)
                               (CL-ARRAY ARR))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.0013809146441856027d0 2.759621819430441d-17)
                        (MULTIPLE-VALUE-LIST (COULOMB-CL 1.0d0 2.5d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         #(0.10842251310207264d0 0.05111086283184191d0
                           0.011428736368066591d0))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((CL (VECTOR-DOUBLE-FLOAT 3)))
                               (COULOMB-CL-ARRAY 0.0d0 1.0d0 CL)
                               (CL-ARRAY CL)))))

