;; Regression test SHUFFLING-SAMPLING for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SHUFFLING-SAMPLING
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(4 3 6 1 5 7 2 8))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0))
                               (V1
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '(1 2 3 4 5 6 7 8))))
                           (SHUFFLE RNG V1)
                           (CL-ARRAY V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(2 3 5 8))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((RNG
                                                                  (MAKE-RANDOM-NUMBER-GENERATOR
                                                                   +MT19937+
                                                                   0))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(1 2 3 4 5
                                                                     6 7 8)))
                                                                 (V2
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :DIMENSIONS
                                                                   4)))
                                                             (CHOOSE-RANDOM RNG
                                                                            V2
                                                                            V1)
                                                             (CL-ARRAY V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #(8 2 3 8 2 4 8 6 5 6))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0))
                               (V1
                                (MAKE-MARRAY '(SIGNED-BYTE 32)
                                             :INITIAL-CONTENTS
                                             '(1 2 3 4 5 6 7 8)))
                               (V2
                                (MAKE-MARRAY '(SIGNED-BYTE 32) :DIMENSIONS 10)))
                           (RANDOM-SAMPLE RNG V2 V1)
                           (CL-ARRAY V2)))))

