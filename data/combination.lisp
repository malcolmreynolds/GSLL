;; Combinations
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2008-02-23 18:49:21EST combination.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Combination structure and CL object
;;;;****************************************************************************

;;; GSL-combination definition
(cffi:defcstruct gsl-combination-c
  (n size)
  (k size)
  (data :pointer))

;;; Allocation, freeing, reading and writing
(defdata "combination" combination unsigned-byte gsl-data 2)

(defmethod gsl-array ((object gsl-combination))
  (foreign-slot-value (pointer object) 'gsl-combination-c 'data))

;;;;****************************************************************************
;;;; Getting values
;;;;****************************************************************************

(defmfun maref ((combination gsl-combination) &rest indices)
  "gsl_combination_get"
  (((pointer combination) :pointer) ((first indices) size))
  :type :method 
  :c-return size
  :documentation			; FDL
  "The ith element of the combination.")

(defmethod data ((object gsl-combination) &optional sequence)
  (let ((seq (or sequence
		 (make-sequence 'list (combination-size object)))))
    (loop for i from 0
	  below (min (length seq) (combination-size object))
	  do (setf (elt seq i) (maref object i)))
    seq))

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defmfun init-first (combination)
  "gsl_combination_init_first"
  (((pointer combination) gsl-combination-c))
  :c-return :void
  :invalidate (combination)
  :documentation			; FDL
  "Initialize the combination c to the lexicographically
      first combination, i.e.  (0,1,2,...,k-1).")

(defmfun init-last (combination)
  "gsl_combination_init_last"
  (((pointer combination) gsl-combination-c))
  :c-return :void
  :invalidate (combination)
  :documentation			; FDL
  "Initialize the combination c to the lexicographically
   last combination, i.e. (n-k,n-k+1,...,n-1).")

(defmfun copy (destination source)
  "gsl_combination_memcpy"
  (((pointer destination) gsl-combination-c)
   ((pointer source) gsl-combination-c))
  :type :method
  :invalidate (destination)
  :documentation			; FDL
  "Copy the elements of the combination source into the
  combination destination.  The two combinations must have the same size.")

;;;;****************************************************************************
;;;; Combination properties
;;;;****************************************************************************

(defmfun combination-range (c)
  "gsl_combination_n"
  (((pointer c) gsl-combination-c))
  :c-return size
  :documentation			; FDL
  "The range (n) of the combination c.")

(defmfun combination-size (c)
  "gsl_combination_k"
  (((pointer c) gsl-combination-c))
  :c-return size
  :documentation			; FDL
  "The number of elements (k) in the combination c.")

#|
;;; Unnecessary, gsl-array serves this function.
(defmfun combination-data (c)
  "gsl_combination_data"
  (((pointer c) gsl-combination-c))
  :c-return :pointer
  :documentation			; FDL
  "A pointer to the array of elements in the combination.")
|#

(defmfun data-valid ((combination gsl-combination))
  "gsl_combination_valid"
  (((pointer combination) :pointer))
  :type :method 
  :c-return :boolean
  :documentation			; FDL
  "Check that the combination is valid.  The k
   elements should lie in the range 0 to n-1, with each
   value occurring once at most and in increasing order.")

;;;;****************************************************************************
;;;; Combination functions
;;;;****************************************************************************

(defmfun combination-next (c)
  "gsl_combination_next" (((pointer c) gsl-combination-c))
  :c-return :success-failure
  :invalidate (c)
  :documentation			; FDL
  "Advance the combination c to the next combination
   in lexicographic order and return T and c.  If no further
   combinations are available it return NIL and c with
   c unmodified.  Starting with the first combination and
   repeatedly applying this function will iterate through all possible
   combinations of a given order.")

(defmfun combination-previous (c)
  "gsl_combination_prev"
  (((pointer c) gsl-combination-c))
  :c-return :success-failure
  :invalidate (c)
  :documentation			; FDL
  "Step backwards from the combination c to the
   previous combination in lexicographic order, returning
   T and c.  If no previous combination is available it returns
   NIL and c with c unmodified.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests combination
 (letm ((comb (combination 4 2 t)))	; combination-range
   (combination-range comb))
 (letm ((comb (combination 4 2 t)))	; combination-size
   (combination-size comb))
 (letm ((comb (combination 4 2 t)))	; init-first, combination-next
   (init-first comb)
   (loop collect (data comb)
	 while (combination-next comb)))
 (letm ((comb (combination 4 2 t)))  ; init-last, combination-previous
   (init-last comb)
   (loop collect (data comb)
	 while (combination-previous comb)))
 (loop for i from 0 to 4		; combination-next
       append
       (letm ((comb (combination 4 i t)))
	 (init-first comb)
	 (loop collect (data comb)
	       while (combination-next comb)))))
|#

(LISP-UNIT:DEFINE-TEST COMBINATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 4)
   (MULTIPLE-VALUE-LIST
    (LETM ((COMB (COMBINATION 4 2 T)))
      (COMBINATION-RANGE COMB))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2)
   (MULTIPLE-VALUE-LIST
    (LETM ((COMB (COMBINATION 4 2 T)))
      (COMBINATION-SIZE COMB))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST (LIST 0 1) (LIST 0 2) (LIST 0 3) (LIST 1 2)
	  (LIST 1 3) (LIST 2 3)))
   (MULTIPLE-VALUE-LIST
    (LETM ((COMB (COMBINATION 4 2 T))) (INIT-FIRST COMB)
	  (LOOP COLLECT (DATA COMB) WHILE
		(COMBINATION-NEXT COMB)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST (LIST 2 3) (LIST 1 3) (LIST 1 2) (LIST 0 3)
	       (LIST 0 2) (LIST 0 1)))
   (MULTIPLE-VALUE-LIST
    (LETM ((COMB (COMBINATION 4 2 T))) (INIT-LAST COMB)
	  (LOOP COLLECT (DATA COMB) WHILE
		(COMBINATION-PREVIOUS COMB)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST (LIST) (LIST 0) (LIST 1) (LIST 2) (LIST 3)
	  (LIST 0 1) (LIST 0 2) (LIST 0 3) (LIST 1 2)
	  (LIST 1 3) (LIST 2 3) (LIST 0 1 2) (LIST 0 1 3)
	  (LIST 0 2 3) (LIST 1 2 3) (LIST 0 1 2 3)))
   (MULTIPLE-VALUE-LIST
    (LOOP FOR I FROM 0 TO 4 APPEND
	  (LETM ((COMB (COMBINATION 4 I T)))
	    (INIT-FIRST COMB)
	    (LOOP COLLECT (DATA COMB) WHILE
		  (COMBINATION-NEXT COMB)))))))

