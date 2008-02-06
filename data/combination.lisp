;; Combinations
;; Liam Healy, Sun Mar 26 2006 - 11:51
;; Time-stamp: <2008-02-04 19:25:57EST combination.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Combination structure and CL object
;;;;****************************************************************************

;;; GSL-combination definition
(cffi:defcstruct gsl-combination-c
  (n :size)
  (k :size)
  (data :pointer))

;;; Allocation, freeing, reading and writing
(defdata "combination" combination unsigned-byte gsl-data 2)

(defmethod gsl-array ((object gsl-combination))
  (foreign-slot-value (pointer object) 'gsl-combination-c 'data))

;;;;****************************************************************************
;;;; Getting values
;;;;****************************************************************************

(defun-gsl gsl-aref ((combination gsl-combination) &rest indices)
  "gsl_combination_get"
  (((pointer combination) :pointer) ((first indices) :size))
  :type :method 
  :c-return :size
  :documentation			; FDL
  "The ith element of the combination.")

(defmethod data ((object gsl-combination) &optional sequence)
  (let ((seq (or sequence
		 (make-sequence 'list (combination-size object)))))
    (loop for i from 0
	  below (min (length seq) (combination-size object))
	  do (setf (elt seq i) (gsl-aref object i)))
    seq))

;;;;****************************************************************************
;;;; Setting values
;;;;****************************************************************************

(defun-gsl init-first (combination)
  "gsl_combination_init_first"
  (((pointer combination) gsl-combination-c))
  :c-return :void
  :invalidate (combination)
  :documentation			; FDL
  "Initialize the combination c to the lexicographically
      first combination, i.e.  (0,1,2,...,k-1).")

(defun-gsl init-last (combination)
  "gsl_combination_init_last"
  (((pointer combination) gsl-combination-c))
  :c-return :void
  :invalidate (combination)
  :documentation			; FDL
  "Initialize the combination c to the lexicographically
   last combination, i.e. (n-k,n-k+1,...,n-1).")

(defun-gsl copy (destination source)
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

(defun-gsl combination-range (c)
  "gsl_combination_n"
  (((pointer c) gsl-combination-c))
  :c-return :size
  :documentation			; FDL
  "The range (n) of the combination c.")

(defun-gsl combination-size (c)
  "gsl_combination_k"
  (((pointer c) gsl-combination-c))
  :c-return :size
  :documentation			; FDL
  "The number of elements (k) in the combination c.")

#|
;;; Unnecessary, gsl-array serves this function.
(defun-gsl combination-data (c)
  "gsl_combination_data"
  (((pointer c) gsl-combination-c))
  :c-return :pointer
  :documentation			; FDL
  "A pointer to the array of elements in the combination.")
|#

(defun-gsl data-valid ((combination gsl-combination))
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

(defun-gsl combination-next (c)
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

(defun-gsl combination-previous (c)
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

(lisp-unit:define-test combination
  (lisp-unit:assert-eql			; combination-range
   4
   (letm ((comb (combination 4 2 t)))
     (combination-range comb)))
  (lisp-unit:assert-eql			; combination-size
   2
   (letm ((comb (combination 4 2 t)))
     (combination-size comb)))
  (lisp-unit:assert-equal		; init-first, combination-next
   '((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))
   (letm ((comb (combination 4 2 t)))
     (init-first comb)
     (loop collect (data comb)
	   while (combination-next comb))))
  (lisp-unit:assert-equal	     ; init-last, combination-previous
   '((2 3) (1 3) (1 2) (0 3) (0 2) (0 1))
   (letm ((comb (combination 4 2 t)))
     (init-last comb)
     (loop collect (data comb)
	   while (combination-previous comb))))
  (lisp-unit:assert-equal		; combination-next
   '(NIL (0) (1) (2) (3) (0 1) (0 2) (0 3) (1 2) (1 3) (2 3)
     (0 1 2) (0 1 3) (0 2 3) (1 2 3) (0 1 2 3))
   (loop for i from 0 to 4
	 append
	 (letm ((comb (combination 4 i t)))
	   (init-first comb)
	   (loop collect (data comb)
		 while (combination-next comb))))))
