;; Regression test SHUFFLING-SAMPLING for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST SHUFFLING-SAMPLING
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(4 3 6 1 5 7 2 8))
   (MULTIPLE-VALUE-LIST
    (let ((rng (make-random-number-generator +mt19937+ 0))
	(v1 #31m(1 2 3 4 5 6 7 8)))
   (cl-array (sample rng 'shuffle :base v1)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(2 3 5 8))
   (MULTIPLE-VALUE-LIST
    (let ((rng (make-random-number-generator +mt19937+ 0))
	(v1 #31m(1 2 3 4 5 6 7 8)))
   (cl-array (sample rng 'choose-random :src v1 :dest 4)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(8 2 3 8 2 4 8 6 5 6))
   (MULTIPLE-VALUE-LIST
    (let ((rng (make-random-number-generator +mt19937+ 0))
	(v1 #31m(1 2 3 4 5 6 7 8)))
   (cl-array (sample rng 'random-sample :src v1 :dest 10))))))
