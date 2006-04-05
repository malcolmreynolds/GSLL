;********************************************************
; file:        permutation.lisp                        
; description: Permutations
; date:        Sun Mar 26 2006 - 11:51                   
; author:      Liam M. Healy                             
; modified:    Wed Apr  5 2006 - 17:35
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Permutation object definition, allocation, reading & writing
;;;;****************************************************************************

;;; GSL-permutation definition
(cffi:defcstruct gsl-permutation-c
  (size :size)
  (data :pointer))

(defclass gsl-permutation (gsl-data)
  ()
  (:documentation "GSL permutation."))

;;; Allocation, freeing, reading and writing
(gsl-data-functions "permutation")

(add-wrap-type gsl-permutation-c (lambda (x) `(pointer ,x)))

#+old
(defmethod set-identity ((permutation gsl-permutation))
  (funcall 
   (defun-gsl :lambda ((permutation gsl-permutation-c))
     "gsl_permutation_init"
     :documentation
     "Initialize the permutation @var{p} to the identity, i.e.
   @math{(0,1,2,@dots{},n-1)}.")
   (pointer permutation)))

(defun-gsl set-identity (((pointer permutation) gsl-permutation-c))
     "gsl_permutation_init"
     :method ((permutation gsl-permutation))
     :documentation
     "Initialize the permutation @var{p} to the identity, i.e.
   @math{(0,1,2,@dots{},n-1)}.")

(defun-gsl permutation-copy
    ((destination gsl-permutation-c) (source gsl-permutation-c) )
  "gsl_permutation_memcpy"
  :documentation
  "Copy the elements of the permutation @var{src} into the
permutation @var{dest}.  The two permutations must have the same size.")

;;;;****************************************************************************
;;;; Accessing elements
;;;;****************************************************************************

#+old
(defmethod gsl-aref ((object gsl-permutation) &rest indices)
  (funcall
   (defun-gsl :lambda ((permutation :pointer) (i :size))
     "gsl_permutation_get"
     :return (:double)
     :c-return-value :return
     :documentation "The ith element of the permutation.")
   (pointer object) (first indices)))

(defun-gsl gsl-aref
    (((pointer permutation) :pointer) ((first indices) :size))
  "gsl_permutation_get"
  :method ((permutation gsl-permutation) &rest indices)
  :return (:double)
  :c-return-value :return
  :documentation "The ith element of the permutation.")

(defun-gsl permutation-swap ((p gsl-permutation-c) (i :size) (j :size))
  "gsl_permutation_swap"
  :documentation
  "Exchanges the @var{i}-th and @var{j}-th elements of the
   permutation @var{p}.")

;;;;****************************************************************************
;;;; Permutation properties
;;;;****************************************************************************

(defun-gsl permutation-size ((p gsl-permutation-c))
  "gsl_permutation_size"
  :c-return-value :return
  :return (:size) 
  :documentation
  "The size of the permutation @var{p}.")

(defun-gsl permutation-data ((p gsl-permutation-c))
  "gsl_permutation_data"
  :c-return-value :return
  :return (:pointer) 
  :documentation
  "A pointer to the array of elements in the
   permutation @var{p}.")

(defun-gsl permutation-valid ((p gsl-permutation-c))
  "gsl_permutation_valid"
  :c-return-value :return
  :return (:boolean) 
  :documentation
  "Check that the permutation @var{p} is valid.  The @var{n}
elements should contain each of the numbers 0 to @math{@var{n}-1} once and only
once.")

;;;;****************************************************************************
;;;; Permutation functions
;;;;****************************************************************************

(defun-gsl permutation-reverse ((p gsl-permutation-c))
  "gsl_permutation_reverse"
  :c-return-value :void
  :documentation
  "Reverse the order of the elements of the permutation @var{p}.")

(defun-gsl permutation-inverse ((inv gsl-permutation-c) (p gsl-permutation-c))
  "gsl_permutation_reverse"
  :documentation
  "Reverse the order of the elements of the permutation @var{p}.")

;; next, prev should return NIL for GSL_FAILURE.

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

#|

|#
