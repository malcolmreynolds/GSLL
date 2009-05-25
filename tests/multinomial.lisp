;; Regression test MULTINOMIAL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MULTINOMIAL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(5 0 1 2))
   (MULTIPLE-VALUE-LIST
    (let ((rng (make-random-number-generator +mt19937+ 0))
	  (p #m(0.1d0 0.2d0 0.3d0 0.4d0)))
      (cl-array (sample rng 'multinomial :sum 8 :probabilities p)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 8.064000000000026d-5)
   (MULTIPLE-VALUE-LIST
    (LET ((P
	   (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
			'(0.1d0 0.2d0 0.3d0 0.4d0)))
	  (N
	   (MAKE-MARRAY '(SIGNED-BYTE 32)
			:INITIAL-CONTENTS '(5 0 1 2))))
      (MULTINOMIAL-PDF P N))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -9.425515753641212d0)
   (MULTIPLE-VALUE-LIST
    (LET ((P
	   (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
			'(0.1d0 0.2d0 0.3d0 0.4d0)))
	  (N
	   (MAKE-MARRAY '(SIGNED-BYTE 32)
			:INITIAL-CONTENTS '(5 0 1 2))))
      (MULTINOMIAL-LOG-PDF P N)))))

