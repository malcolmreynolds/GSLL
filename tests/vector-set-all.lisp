;; Regression test VECTOR-SET-ALL for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-SET-ALL
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST #(-34.5 -34.5 -34.5))
   (MULTIPLE-VALUE-LIST
    (LET ((V1 (MAKE-MARRAY 'SINGLE-FLOAT :DIMENSIONS '3)))
      (CL-ARRAY (SET-ALL V1 -34.5)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST #(-34.5d0 -34.5d0 -34.5d0))
   (MULTIPLE-VALUE-LIST
    (LET ((V1 (MAKE-MARRAY 'DOUBLE-FLOAT :DIMENSIONS '3)))
      (CL-ARRAY (SET-ALL V1 -34.5d0)))))
  #+fsbv
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST #(#C(-34.5 8.24) #C(-34.5 8.24) #C(-34.5 8.24)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
			:DIMENSIONS '3)))
      (CL-ARRAY (SET-ALL V1 #C(-34.5 8.24))))))
  #+fsbv
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    #(#C(-34.5d0 8.24d0) #C(-34.5d0 8.24d0)
      #C(-34.5d0 8.24d0)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
			:DIMENSIONS '3)))
      (CL-ARRAY (SET-ALL V1 #C(-34.5d0 8.24d0))))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST #(-64 -64 -64))
				    (MULTIPLE-VALUE-LIST
				     (LET ((V1
					    (MAKE-MARRAY
					     '(SIGNED-BYTE
					       8)
					     :DIMENSIONS
					     '3)))
				       (CL-ARRAY
					(SET-ALL V1
						 -64)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST #(67 67 67))
				    (MULTIPLE-VALUE-LIST
				     (LET ((V1
					    (MAKE-MARRAY
					     '(UNSIGNED-BYTE
					       8)
					     :DIMENSIONS
					     '3)))
				       (CL-ARRAY
					(SET-ALL V1
						 67)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST #(-64 -64 -64))
				    (MULTIPLE-VALUE-LIST
				     (LET ((V1
					    (MAKE-MARRAY
					     '(SIGNED-BYTE
					       16)
					     :DIMENSIONS
					     '3)))
				       (CL-ARRAY
					(SET-ALL V1
						 -64)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST #(67 67 67))
				    (MULTIPLE-VALUE-LIST
				     (LET ((V1
					    (MAKE-MARRAY
					     '(UNSIGNED-BYTE
					       16)
					     :DIMENSIONS
					     '3)))
				       (CL-ARRAY
					(SET-ALL V1
						 67)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST #(-64 -64 -64))
				    (MULTIPLE-VALUE-LIST
				     (LET ((V1
					    (MAKE-MARRAY
					     '(SIGNED-BYTE
					       32)
					     :DIMENSIONS
					     '3)))
				       (CL-ARRAY
					(SET-ALL V1
						 -64)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL (LIST #(67 67 67))
				    (MULTIPLE-VALUE-LIST
				     (LET ((V1
					    (MAKE-MARRAY
					     '(UNSIGNED-BYTE
					       32)
					     :DIMENSIONS
					     '3)))
				       (CL-ARRAY
					(SET-ALL V1
						 67)))))
  #+int64
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST #(-64 -64 -64))
   (MULTIPLE-VALUE-LIST
    (LET ((V1 (MAKE-MARRAY '(SIGNED-BYTE 64) :DIMENSIONS '3)))
      (CL-ARRAY (SET-ALL V1 -64)))))
  #+int64
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST #(67 67 67))
   (MULTIPLE-VALUE-LIST
    (LET ((V1 (MAKE-MARRAY '(UNSIGNED-BYTE 64) :DIMENSIONS '3)))
      (CL-ARRAY (SET-ALL V1 67))))))
