;; Discrete Hankel Transforms.
;; Liam Healy, Sat Dec  8 2007 - 16:50
;; Time-stamp: <2009-03-02 16:22:06EST hankel.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_dht.h

;;; Everything compiles, but not tested -- need example.
;;; We don't use gsl_dht_new = gsl_dht_alloc + gsl_dht_init.

;;; Create the Hankel transform object and essential methods/functions.
(defmobject hankel "gsl_dht"
  ((size sizet))
  "discrete Hankel Transform"
  :documentation
  "Allocate a Discrete Hankel transform object of the given size and
   optionally initialize the transform for the given values of nu and x."
  :initialize-suffix "init"
  :initialize-args ((nu :double) (xmax :double)))

(defmfun apply-hankel
    (hankel array-in
     &optional
     (array-out (make-marray 'double-float :dimensions (dimensions array-in))))
  "gsl_dht_apply"
  (((mpointer hankel) :pointer) ((c-pointer array-in) :pointer)
   ((c-pointer array-out) :pointer))
  :inputs (array-in)
  :outputs (array-out)
  :return (array-out)
  :documentation			; FDL
  "Apply the transform to the array array-in
   whose size is equal to the size of the transform.  The result is stored
   in the array array-out which must be of the same length.")

(defmfun sample-x-hankel (hankel n)
  "gsl_dht_x_sample"
  (((mpointer hankel) :pointer) (n :int))
  :c-return :double
  :documentation			; FDL
  "The value of the n-th sample point in the unit
  interval, (j_{\nu,n+1}/j_{\nu,M}) X, which are the
  points where the function f(t) is assumed to be sampled.")

(defmfun sample-k-hankel (hankel n)
  "gsl_dht_k_sample"
  (((mpointer hankel) :pointer) (n :int))
  :c-return :double
  :documentation			; FDL
  "The value of the n-th sample point in k-space, j_{\nu,n+1}/X}.")


(save-test hankel					; tests from dht/test.c
 (copy (apply-hankel (make-hankel 3 1.0d0 1.0d0) #m(1.0d0 2.0d0 3.0d0)) 'array)
 ;; Exact, forward-inverse transform should be accurate to 2e-5
 (let ((hank (make-hankel 3 1.0d0 1.0d0)))
   (copy
    (elt* (expt (bessel-zero-j1 4) 2)	; inverse transformation
	  (apply-hankel
	   hank (apply-hankel hank #m(1.0d0 2.0d0 3.0d0))))
    'array))
 ;; Simple
 (let ((hank (make-hankel 128 0.0d0 100.0d0))
       (in (make-marray 'double-float :dimensions 128)))
   (loop for n from 0 below 128
      do (setf (maref in n)
	       (/ (1+ (expt (sample-x-hankel hank n) 2)))))
   (copy (apply-hankel hank in) 'array))
 ;; Integrate[ x exp(-x) J_1(a x), {x,0,Inf}] = a F(3/2, 2; 2; -a^2)
 ;; expected accuracy only about 2%
 (let ((hank (make-hankel 128 1.0d0 20.0d0))
       (in (make-marray 'double-float :dimensions 128)))
   (loop for n from 0 below 128
      do (setf (maref in n) (exp (- (sample-x-hankel hank n)))))
   (copy (apply-hankel hank in) 'array))
 ;; Integrate[ x^2 (1-x^2) J_1(a x), {x,0,1}] = 2/a^2 J_3(a)
 (let ((hank (make-hankel 128 1.0d0 1.0d0))
       (in (make-marray 'double-float :dimensions 128)))
   (loop for n from 0 below 128
      do (setf (maref in n)
	       (let ((x (sample-x-hankel hank n)))
		 (* x (- 1 (expt x 2))))))
   (copy (apply-hankel hank in) 'array)))
