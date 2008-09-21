;; Combinations
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2008-09-20 22:04:48EDT combination.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Combination structure and CL object
;;;;****************************************************************************

;;; GSL-combination definition
(cffi:defcstruct gsl-combination-c
  (n sizet)
  (k sizet)
  (data :pointer))

(defclass combination
    (#+sizet-64 vector-unsigned-byte-64
     #+sizet-32 vector-unsigned-byte-32)
  ()
  (:documentation "GSL permutations."))

;;; The following three forms take the place of a data-defclass call

(defmethod letm-expansion
    (symbol (type (eql 'combination)) args body)
  (expand-data symbol type (first args) (second args) body))

(arglist-only combination "A combination." n k sync-array-on-exit)

(defun make-data-combination (nk)
  "Make the combination object with the data array."
  ;; The replaces the function of make-data-from-array and
  ;; make-data-from-dimensions because the class needs different
  ;; initialization than other kinds of data.
  (let ((k (second nk)))
    (make-instance
     'combination
     :cl-array (make-array* k *sizet-type*)
     :mpointer nil	   ; this will be set by :before method below.
     :c-pointer nil			; this will be set by defmfun
     :dimensions (copy-list nk)
     :total-size k)))

(defmethod mpointer :before ((object combination))
  "Make a GSL struct if there isn't one already."
  (unless (slot-value object 'mpointer)
    ;; Cribbed from alloc-gsl-struct
    (unless (c-pointer object) (error "No C array.")) ; safety while developing
    (let ((blockptr (cffi:foreign-alloc 'gsl-combination-c)))
      (setf (block-pointer object)
	    blockptr
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'data)
	    (c-pointer object)
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'n)
	    (elt (dimensions object) 0)
	    (cffi:foreign-slot-value blockptr 'gsl-combination-c 'k)
	    (elt (dimensions object) 1)
	    (mpointer object)
	    (block-pointer object)))
    nil))

;;; The total-size of a combination is k, because that is the length
;;; of the vector that represents it.

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defmfun init-first (combination)
  "gsl_combination_init_first"
  (((mpointer combination) :pointer))
  :c-return :void
  :inputs (combination)
  :outputs (combination)
  :documentation			; FDL
  "Initialize the combination c to the lexicographically
      first combination, i.e.  (0,1,2,...,k-1).")

(defmfun init-last (combination)
  "gsl_combination_init_last"
  (((mpointer combination) :pointer))
  :c-return :void
  :inputs (combination)
  :outputs (combination)
  :documentation			; FDL
  "Initialize the combination c to the lexicographically
   last combination, i.e. (n-k,n-k+1,...,n-1).")

(defmfun copy (destination source)
  "gsl_combination_memcpy"
  (((mpointer destination) :pointer)
   ((mpointer source) :pointer))
  :definition :method
  :inputs (source)
  :outputs (destination)
  :documentation			; FDL
  "Copy the elements of the combination source into the
  combination destination.  The two combinations must have the same size.")

;;;;****************************************************************************
;;;; Combination properties
;;;;****************************************************************************

(defmfun combination-range (c)
  "gsl_combination_n"
  (((mpointer c) :pointer))
  :c-return sizet
  :inputs (c)
  :documentation			; FDL
  "The range (n) of the combination c.")

(defmfun combination-size (c)
  "gsl_combination_k"
  (((mpointer c) :pointer))
  :c-return sizet
  :inputs (c)
  :documentation			; FDL
  "The number of elements (k) in the combination c.")

#|
;;; Unnecessary, gsl-array serves this function.
(defmfun combination-data (c)
  "gsl_combination_data"
  (((mpointer c) :pointer))
  :c-return :pointer
  :documentation			; FDL
  "A pointer to the array of elements in the combination.")
|#

(defmfun data-valid ((combination combination))
  "gsl_combination_valid"
  (((mpointer combination) :pointer))
  :definition :method 
  :c-return :boolean
  :documentation			; FDL
  "Check that the combination is valid.  The k
   elements should lie in the range 0 to n-1, with each
   value occurring once at most and in increasing order.")

;;;;****************************************************************************
;;;; Combination functions
;;;;****************************************************************************

(defmfun combination-next (c)
  "gsl_combination_next" (((mpointer c) :pointer))
  :c-return :success-failure
  :inputs (c)
  :outputs (c)
  :documentation			; FDL
  "Advance the combination c to the next combination
   in lexicographic order and return T and c.  If no further
   combinations are available it return NIL and c with
   c unmodified.  Starting with the first combination and
   repeatedly applying this function will iterate through all possible
   combinations of a given order.")

(defmfun combination-previous (c)
  "gsl_combination_prev"
  (((mpointer c) :pointer))
  :c-return :success-failure
  :inputs (c)
  :outputs (c)
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
 (letm ((comb (combination '(4 2) t)))	; combination-range
   (combination-range comb))
 (letm ((comb (combination '(4 2) t)))	; combination-size
   (combination-size comb))
 (letm ((comb (combination '(4 2) t)))	; init-first, combination-next
   (init-first comb)
   (loop collect (copy-seq (cl-array comb))
	 while (combination-next comb)))
 (letm ((comb (combination '(4 2) t)))  ; init-last, combination-previous
   (init-last comb)
   (loop collect (copy-seq (cl-array comb))
	 while (combination-previous comb)))
 (loop for i from 0 to 4		; combination-next
       append
       (letm ((comb (combination (list 4 i) t)))
	 (init-first comb)
	 (loop collect (copy-seq (cl-array comb))
	       while (combination-next comb)))))
|#

(LISP-UNIT:DEFINE-TEST COMBINATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 4)
   (MULTIPLE-VALUE-LIST
    (LETM ((COMB (COMBINATION '(4 2) T)))
      (COMBINATION-RANGE COMB))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2)
   (MULTIPLE-VALUE-LIST
    (LETM ((COMB (COMBINATION '(4 2) T)))
      (COMBINATION-SIZE COMB))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(0 1) #(0 2) #(0 3) #(1 2) #(1 3) #(2 3)))
   (MULTIPLE-VALUE-LIST
    (LETM ((COMB (COMBINATION '(4 2) T)))
      (INIT-FIRST COMB)
      (LOOP COLLECT (COPY-SEQ (CL-ARRAY COMB)) WHILE
	   (COMBINATION-NEXT COMB)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(2 3) #(1 3) #(1 2) #(0 3) #(0 2) #(0 1)))
   (MULTIPLE-VALUE-LIST
    (LETM ((COMB (COMBINATION '(4 2) T))) (INIT-LAST COMB)
	  (LOOP COLLECT (COPY-SEQ (CL-ARRAY COMB)) WHILE
	       (COMBINATION-PREVIOUS COMB)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST #() #(0) #(1) #(2) #(3) #(0 1) #(0 2) #(0 3)
	  #(1 2) #(1 3) #(2 3) #(0 1 2) #(0 1 3) #(0 2 3)
	  #(1 2 3) #(0 1 2 3)))
   (MULTIPLE-VALUE-LIST
    (LOOP FOR I FROM 0 TO 4 APPEND
	 (LETM ((COMB (COMBINATION (LIST 4 I) T)))
	   (INIT-FIRST COMB)
	   (LOOP COLLECT (COPY-SEQ (CL-ARRAY COMB))
	      WHILE (COMBINATION-NEXT COMB)))))))


