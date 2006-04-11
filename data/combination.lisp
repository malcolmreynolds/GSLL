;********************************************************
; file:        combination.lisp                        
; description: Combinations
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Tue Apr 11 2006 - 13:15
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Combination object definition, allocation, reading & writing
;;;;****************************************************************************

;;; GSL-combination definition
(cffi:defcstruct gsl-combination-c
  (n :size)
  (k :size)
  (data :pointer))

(defclass gsl-combination (gsl-data)
  ((base-type :initform :size :reader base-type :allocation :class))
  (:documentation "GSL combination."))

;;; Allocation, freeing, reading and writing
(gsl-data-functions "combination" 2)

(add-wrap-type gsl-combination-c (lambda (x) `(pointer ,x)))

(defun-gsl init-first ((combination gsl-combination-c))
  "gsl_combination_init_first"
  :c-return-value :void
  :documentation
  "Initialize the combination @var{c} to the lexicographically
      first combination, i.e.  @math{(0,1,2,@dots{},k-1)}.")

(defun-gsl init-last ((combination gsl-combination-c))
  "gsl_combination_init_last"
  :c-return-value :void
  :documentation
  "Initialize the combination @var{c} to the lexicographically
   last combination, i.e.  @math{(n-k,n-k+1,@dots{},n-1)}.")

(defun-gsl combination-copy
    ((destination gsl-combination-c) (source gsl-combination-c) )
  "gsl_combination_memcpy"
  :documentation
  "Copy the elements of the combination @var{src} into the
  combination @var{dest}.  The two combinations must have the same size.")

;;;;****************************************************************************
;;;; Accessing combination elements
;;;;****************************************************************************

(defun-gsl gsl-aref
    (((pointer combination) :pointer) ((first indices) :size))
  "gsl_combination_get"
  :method ((combination gsl-combination) &rest indices)
  :return (:size)
  :c-return-value :return
  :documentation "The ith element of the combination.")

(defmethod data-export ((combination gsl-combination))
  "The combination as a list."
  (loop for j below (combination-size combination)
	collect	(gsl-aref combination j)))

;;;;****************************************************************************
;;;; Combination properties
;;;;****************************************************************************

(defun-gsl combination-range ((c gsl-combination-c))
  "gsl_combination_n"
  :c-return-value :return
  :return (:size) 
  :documentation
  "The range (@math{n}) of the combination @var{c}.")

(defun-gsl combination-size ((c gsl-combination-c))
  "gsl_combination_k"
  :c-return-value :return
  :return (:size) 
  :documentation
  "The number of elements (@math{k}) in the combination @var{c}.")

(defun-gsl combination-data ((c gsl-combination-c))
  "gsl_combination_data"
  :c-return-value :return
  :return (:pointer) 
  :documentation
  "A pointer to the array of elements in the combination @var{p}.")

(defun-gsl data-valid (((pointer combination) :pointer))
  "gsl_combination_valid"
  :method ((combination gsl-combination))
  :c-return-value :return
  :return (:boolean) 
  :documentation
  "Check that the combination @var{c} is valid.  The @var{k}
   elements should lie in the range 0 to @math{@var{n}-1}, with each
   value occurring once at most and in increasing order.")

;;;;****************************************************************************
;;;; Combination functions
;;;;****************************************************************************

(defun-gsl combination-next ((c gsl-combination-c))
  "gsl_combination_next"
  :c-return-value :success-failure
  :documentation
  "Advance the combination @var{c} to the next combination
   in lexicographic order and return T.  If no further
   combinations are available it return NIL and leave
   @var{c} unmodified.  Starting with the first combination and
   repeatedly applying this function will iterate through all possible
   combinations of a given order.")

(defun-gsl combination-prev ((c gsl-combination-c))
  "gsl_combination_prev"
  :c-return-value :success-failure
  :documentation
  "Step backwards from the combination @var{c} to the
   previous combination in lexicographic order, returning
   T.  If no previous combination is available it returns
   NIL and leaves @var{c} unmodified.")

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

#|
;;; This is the example in the GSL manual, but we do not use
;;; the C streams for output, rather we use gsl-aref.

(loop for i from 0 to 4
      do
      (with-data (comb combination (4 i) t)
	(loop do
	      (princ "{")
	      (loop for j below (combination-size comb) do
		    (princ (gsl-aref comb j)))
	      (princ "}")
	      while (combination-next comb))))
{}{0}{1}{2}{3}{01}{02}{03}{12}{13}{23}{012}{013}{023}{123}{0123}

(loop for i from 0 to 4
      append
      (with-data (comb combination (4 i) t)
	(loop collect (combination-list comb)
	      while (combination-next comb))))

(NIL (0) (1) (2) (3) (0 1) (0 2) (0 3) (1 2) (1 3) (2 3)
     (0 1 2) (0 1 3) (0 2 3) (1 2 3) (0 1 2 3))

|#
