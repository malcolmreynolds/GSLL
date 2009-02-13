;; Regression test VECTOR-COPY-TO-CL-AND-BACK for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-COPY-TO-CL-AND-BACK
    (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
     (LIST #(-34.5 8.24 3.29))
     (MULTIPLE-VALUE-LIST
      (CL-ARRAY
       (COPY
	(COPY
	 (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
		      '(-34.5 8.24 3.29))
	 'ARRAY)
	'SINGLE-FLOAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(-34.5d0 8.24d0 3.29d0))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
		    '(-34.5d0 8.24d0 3.29d0))
       'ARRAY)
      'DOUBLE-FLOAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
		    :INITIAL-CONTENTS
		    '(-34.5 8.24 3.29 -8.93 34.12 -6.15))
       'ARRAY)
      '(COMPLEX SINGLE-FLOAT)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0)
      #C(34.12d0 -6.15d0)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
		    :INITIAL-CONTENTS
		    '(-34.5d0 8.24d0 3.29d0 -8.93d0
		      34.12d0 -6.15d0))
       'ARRAY)
      '(COMPLEX DOUBLE-FLOAT)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-64 -68 71))
				     (MULTIPLE-VALUE-LIST
				      (CL-ARRAY
				       (COPY
					(COPY
					 (MAKE-MARRAY
					  '(SIGNED-BYTE 8)
					  :INITIAL-CONTENTS
					  '(-64 -68 71))
					 'ARRAY)
					'(SIGNED-BYTE
					  8)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(67 44 189))
				     (MULTIPLE-VALUE-LIST
				      (CL-ARRAY
				       (COPY
					(COPY
					 (MAKE-MARRAY
					  '(UNSIGNED-BYTE
					    8)
					  :INITIAL-CONTENTS
					  '(67 44 189))
					 'ARRAY)
					'(UNSIGNED-BYTE
					  8)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-64 -68 71))
				     (MULTIPLE-VALUE-LIST
				      (CL-ARRAY
				       (COPY
					(COPY
					 (MAKE-MARRAY
					  '(SIGNED-BYTE
					    16)
					  :INITIAL-CONTENTS
					  '(-64 -68 71))
					 'ARRAY)
					'(SIGNED-BYTE
					  16)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(67 44 189))
				     (MULTIPLE-VALUE-LIST
				      (CL-ARRAY
				       (COPY
					(COPY
					 (MAKE-MARRAY
					  '(UNSIGNED-BYTE
					    16)
					  :INITIAL-CONTENTS
					  '(67 44 189))
					 'ARRAY)
					'(UNSIGNED-BYTE
					  16)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-64 -68 71))
				     (MULTIPLE-VALUE-LIST
				      (CL-ARRAY
				       (COPY
					(COPY
					 (MAKE-MARRAY
					  '(SIGNED-BYTE
					    32)
					  :INITIAL-CONTENTS
					  '(-64 -68 71))
					 'ARRAY)
					'(SIGNED-BYTE
					  32)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(67 44 189))
				     (MULTIPLE-VALUE-LIST
				      (CL-ARRAY
				       (COPY
					(COPY
					 (MAKE-MARRAY
					  '(UNSIGNED-BYTE
					    32)
					  :INITIAL-CONTENTS
					  '(67 44 189))
					 'ARRAY)
					'(UNSIGNED-BYTE
					  32)))))
  #+int64
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(-64 -68 71))
				     (MULTIPLE-VALUE-LIST
				      (CL-ARRAY
				       (COPY
					(COPY
					 (MAKE-MARRAY
					  '(SIGNED-BYTE
					    64)
					  :INITIAL-CONTENTS
					  '(-64 -68 71))
					 'ARRAY)
					'(SIGNED-BYTE
					  64)))))
  #+int64
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(67 44 189))
				     (MULTIPLE-VALUE-LIST
				      (CL-ARRAY
				       (COPY
					(COPY
					 (MAKE-MARRAY
					  '(UNSIGNED-BYTE
					    64)
					  :INITIAL-CONTENTS
					  '(67 44 189))
					 'ARRAY)
					'(UNSIGNED-BYTE
					  64))))))

