;; Discrete random variables
;; Liam Healy, Sat Nov 11 2006 - 21:51
;; Time-stamp: <2008-01-26 17:22:14EST discrete.lisp>
;; $Id: $

(in-package :gsl)

(cffi:defcstruct discrete-t
  "Structure for Walker algorithm."
  (K :size)
  (A :pointer)
  (F :pointer))

(set-asf discrete discrete-preprocess discrete-free)

(defun-gsl discrete-preprocess (probabilities) 
  "gsl_ran_discrete_preproc"
  (((dim0 probabilities) :size) ((gsl-array probabilities) :pointer))
  :c-return :pointer
  :export nil
  :index (with-gsl-objects discrete)
  :documentation
  "A pointer to a structure that contains the lookup
  table for the discrete random number generator.  The array probabilities contains
  the probabilities of the discrete events; these array elements must all be 
  positive, but they needn't add up to one (so you can think of them more
  generally as ``weights'')---the preprocessor will normalize appropriately.
  This return value is used as an argument to #'discrete.")

(defun-gsl discrete-free (table)
  "gsl_ran_discrete_free" ((table :pointer))
  :c-return :void
  :export nil
  :index (with-gsl-objects discrete)
  :documentation
  "De-allocates the lookup table created by #'discrete-preprocess.")

(export 'with-discrete-table)
(defmacro with-discrete-table ((probabilities table) &body body)
  `(let ((,table (discrete-preprocess ,probabilities)))
     (unwind-protect
	  (progn ,@body)
       (discrete-free ,table))))

(defun-gsl discrete (generator table)
  "gsl_ran_discrete"
  (((generator generator) :pointer) (table :pointer))
  :c-return :size
  :documentation
  "Generate discrete random numbers after running #'discrete-preprocess;
   the argument 'table is the value returned by #'discrete-preprocess.")

(defun-gsl discrete-pdf (k table)
  "gsl_ran_discrete_pdf"
  ((k :size) (table :pointer))
  :c-return :double
  :documentation
  "The probability @math{P[k]} of observing the variable @var{k}.
   Since @math{P[k]} is not stored as part of the lookup table, it must be
   recomputed; this computation takes @math{O(K)}, so if @var{K} is large
   and you care about the original array @math{P[k]} used to create the
   lookup table, then you should just keep this original array @math{P[k]}
   around.")

;;; Examples and unit test
(lisp-unit:define-test discrete
  (lisp-unit:assert-equal
   '(1 0 1 1 0 1 1 2 1 2 2)
   (with-data (probabilities vector-double 3)
     (setf (data probabilities) #(0.25d0 0.5d0 0.25d0))
     (with-gsl-objects ((discrete table probabilities))
       (rng-set *rng-mt19937* 0)
       (loop for i from 0 to 10
	     collect
	     (discrete *rng-mt19937* table))))
   (lisp-unit:assert-first-fp-equal
    "0.500000000000d+00"
    (with-data (probabilities vector-double 3)
      (setf (data probabilities)
	    #(0.25d0 0.5d0 0.25d0))
      (with-gsl-objects ((discrete table probabilities))
	(discrete-pdf
	 1
	 table))))))
