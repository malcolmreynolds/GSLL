;; Regression test MATRIX-COPY-TO-CL-AND-BACK for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST MATRIX-COPY-TO-CL-AND-BACK
    (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
     (LIST
      #2A((-34.5 8.24 3.29)
	  (-8.93 34.12 -6.15)
	  (49.27 -13.49 32.5)))
     (MULTIPLE-VALUE-LIST
      (CL-ARRAY
       (COPY
	(COPY
	 (MAKE-MARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS
		      '((-34.5 8.24 3.29)
			(-8.93 34.12 -6.15)
			(49.27 -13.49 32.5)))
	 'ARRAY)
	'SINGLE-FLOAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((-34.5d0 8.24d0 3.29d0)
	(-8.93d0 34.12d0 -6.15d0)
	(49.27d0 -13.49d0 32.5d0)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS
		    '((-34.5d0 8.24d0 3.29d0)
		      (-8.93d0 34.12d0 -6.15d0)
		      (49.27d0 -13.49d0 32.5d0)))
       'ARRAY)
      'DOUBLE-FLOAT))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15))
	(#C(-8.93 34.12) #C(-6.15 49.27) #C(-13.49 32.5))
	(#C(49.27 -13.49) #C(32.5 42.73)
	   #C(-17.24 43.31))))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(COMPLEX SINGLE-FLOAT)
		    :INITIAL-CONTENTS
		    '((-34.5 8.24 3.29 -8.93 34.12 -6.15)
		      (-8.93 34.12 -6.15 49.27 -13.49
		       32.5)
		      (49.27 -13.49 32.5 42.73 -17.24
		       43.31)))
       'ARRAY)
      '(COMPLEX SINGLE-FLOAT)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0)
	   #C(34.12d0 -6.15d0))
	(#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0)
	   #C(-13.49d0 32.5d0))
	(#C(49.27d0 -13.49d0) #C(32.5d0 42.73d0)
	   #C(-17.24d0 43.31d0))))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(COMPLEX DOUBLE-FLOAT)
		    :INITIAL-CONTENTS
		    '((-34.5d0 8.24d0 3.29d0 -8.93d0
		       34.12d0 -6.15d0)
		      (-8.93d0 34.12d0 -6.15d0 49.27d0
		       -13.49d0 32.5d0)
		      (49.27d0 -13.49d0 32.5d0 42.73d0
		       -17.24d0 43.31d0)))
       'ARRAY)
      '(COMPLEX DOUBLE-FLOAT)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((-64 -68 71) (-91 52 -10) (73 -5 123)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(SIGNED-BYTE 8) :INITIAL-CONTENTS
		    '((-64 -68 71) (-91 52 -10)
		      (73 -5 123)))
       'ARRAY)
      '(SIGNED-BYTE 8)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((67 44 189) (116 163 140) (161 215 98)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(UNSIGNED-BYTE 8) :INITIAL-CONTENTS
		    '((67 44 189) (116 163 140)
		      (161 215 98)))
       'ARRAY)
      '(UNSIGNED-BYTE 8)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((-64 -68 71) (-91 52 -10) (73 -5 123)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(SIGNED-BYTE 16) :INITIAL-CONTENTS
		    '((-64 -68 71) (-91 52 -10)
		      (73 -5 123)))
       'ARRAY)
      '(SIGNED-BYTE 16)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((67 44 189) (116 163 140) (161 215 98)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(UNSIGNED-BYTE 16) :INITIAL-CONTENTS
		    '((67 44 189) (116 163 140)
		      (161 215 98)))
       'ARRAY)
      '(UNSIGNED-BYTE 16)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((-64 -68 71) (-91 52 -10) (73 -5 123)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(SIGNED-BYTE 32) :INITIAL-CONTENTS
		    '((-64 -68 71) (-91 52 -10)
		      (73 -5 123)))
       'ARRAY)
      '(SIGNED-BYTE 32)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((67 44 189) (116 163 140) (161 215 98)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(UNSIGNED-BYTE 32) :INITIAL-CONTENTS
		    '((67 44 189) (116 163 140)
		      (161 215 98)))
       'ARRAY)
      '(UNSIGNED-BYTE 32)))))
  #+int64
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((-64 -68 71) (-91 52 -10) (73 -5 123)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(SIGNED-BYTE 64) :INITIAL-CONTENTS
		    '((-64 -68 71) (-91 52 -10)
		      (73 -5 123)))
       'ARRAY)
      '(SIGNED-BYTE 64)))))
  #+int64
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #2A((67 44 189) (116 163 140) (161 215 98)))
   (MULTIPLE-VALUE-LIST
    (CL-ARRAY
     (COPY
      (COPY
       (MAKE-MARRAY '(UNSIGNED-BYTE 64) :INITIAL-CONTENTS
		    '((67 44 189) (116 163 140)
		      (161 215 98)))
       'ARRAY)
      '(UNSIGNED-BYTE 64))))))

