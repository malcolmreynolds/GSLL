;; Regression test PERMUTATION for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST PERMUTATION
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 2)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T)))
                                                             (MAREF PERM-1
                                                                    2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 1 2 3))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T)))
                                                             (CL-ARRAY
                                                              PERM-1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(3 2 1 0))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T)))
                                                             (SET-IDENTITY
                                                              PERM-1)
                                                             (CL-ARRAY
                                                              (PERMUTATION-REVERSE
                                                               PERM-1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 3 1 2))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T))
                                                                 (PERM-2
                                                                  (MAKE-PERMUTATION
                                                                   4 T)))
                                                             (SET-IDENTITY
                                                              PERM-1)
                                                             (PERMUTATION-NEXT
                                                              PERM-1)
                                                             (PERMUTATION-NEXT
                                                              PERM-1)
                                                             (PERMUTATION-NEXT
                                                              PERM-1)
                                                             (PERMUTATION-INVERSE
                                                              PERM-2 PERM-1)
                                                             (CL-ARRAY
                                                              PERM-2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(0 3 2 1))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T)))
                                                             (SET-IDENTITY
                                                              PERM-1)
                                                             (SWAP-ELEMENTS
                                                              PERM-1 1 3)
                                                             (CL-ARRAY
                                                              PERM-1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(33 44 11 22))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T))
                                                                 (INTVEC
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(11 22 33
                                                                     44))))
                                                             (SET-IDENTITY
                                                              PERM-1)
                                                             (SWAP-ELEMENTS
                                                              PERM-1 1 3)
                                                             (SWAP-ELEMENTS
                                                              PERM-1 0 2)
                                                             (PERMUTE-VECTOR
                                                              PERM-1 INTVEC)
                                                             (CL-ARRAY
                                                              INTVEC))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 3)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T)))
                                                             (SET-IDENTITY
                                                              PERM-1)
                                                             (SWAP-ELEMENTS
                                                              PERM-1 1 3)
                                                             (INVERSIONS
                                                              PERM-1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 3)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T)))
                                                             (SET-IDENTITY
                                                              PERM-1)
                                                             (SWAP-ELEMENTS
                                                              PERM-1 1 3)
                                                             (LINEAR-CYCLES
                                                              PERM-1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 2)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((PERM-1
                                                                  (MAKE-PERMUTATION
                                                                   4 T)))
                                                             (SET-IDENTITY
                                                              PERM-1)
                                                             (SWAP-ELEMENTS
                                                              PERM-1 1 3)
                                                             (SWAP-ELEMENTS
                                                              PERM-1 0 2)
                                                             (CANONICAL-CYCLES
                                                              PERM-1)))))

