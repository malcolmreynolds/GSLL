;; Regression test RANDOM-NUMBER-GENERATORS for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST RANDOM-NUMBER-GENERATORS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 999 162 282 947 231 484 957 744 540 739 759))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (UNIFORM-FIXNUM RNG 1000)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.11177622997750353d0 0.9591667949963206d0
                               0.8415268011584537d0 0.9254037136795947d0
                               0.27540698474059205d0 0.7093040573919677d0
                               0.5541333041871588d0 0.8806957769583426d0
                               0.597139396982798d0 0.7518741133398722d0
                               0.9311084621265104d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR *CMRG* 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT (UNIFORM RNG))))))

