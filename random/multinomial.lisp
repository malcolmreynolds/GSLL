;********************************************************
; file:        multinomial.lisp                          
; description: Multinomial distribution
; date:        Sat Nov 25 2006 - 16:00
; author:      Liam M. Healy                             
; modified:    Sat Nov 25 2006 - 21:16
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl multinomial (generator sum p n)
  "gsl_ran_multinomial"
  (((generator generator) :pointer) 
   ((dim0 p) :size)
   (sum :size)
   ((gsl-array p) :pointer)
   ;; technically, n should be a uint array, but integers work
   ((gsl-array n) :pointer))
  :c-return :void
  :documentation
  "Returns an array n of @var{K} random variates from a 
   multinomial distribution.  The sum of the array n is specified
   by sum=N.  The distribution function is
   P(n_1, n_2, ..., n_K) = 
   (N!/(n_1! n_2! ... n_K!)) p_1^n_1 p_2^n_2 ... p_K^n_K
   where @math{(n_1, n_2, ..., n_K)} 
   are nonnegative integers with 
   @math{sum_@{k=1@}^K n_k = N},
   and @math{(p_1, p_2, ..., p_K)}
   is a probability distribution with @math{\sum p_i = 1}.  
   If the array @var{p}[@var{K}] is not normalized then its entries will be
   treated as weights and normalized appropriately.
   Random variates are generated using the conditional binomial method (see
   C.S. David, @cite{The computer generation of multinomial random
   variates}, Comp. Stat. Data Anal. 16 (1993) 205--217 for details).")

(defun-gsl multinomial-pdf (p n)
  "gsl_ran_multinomial_pdf"
  (((dim0 p) :uint) ((gsl-array p) :pointer) ((gsl-array n) :pointer))
  :c-return :double
  :documentation
  "Compute the probability @math{P(n_1, n_2, ..., n_K)}
   of sampling @var{n}[@var{K}] from a multinomial distribution 
   with parameters @var{p}[@var{K}], using the formula given for #'multinomial.")

(defun-gsl multinomial-log-pdf (p n)
  "gsl_ran_multinomial_lnpdf"
  (((dim0 p) :uint) ((gsl-array p) :pointer) ((gsl-array n) :pointer))
  :c-return :double
  :documentation
  "Compute the natural logarithm of the probability @math{P(n_1, n_2, ..., n_K)}
   of sampling @var{n}[@var{K}] from a multinomial distribution 
   with parameters @var{p}[@var{K}], using the formula given for #'multinomial.")

;;; Examples and unit test
(lisp-unit:define-test multinomial
  (lisp-unit:assert-equalp
   #(5 0 1 2)
   (progn
     (rng-set *rng-mt19937* 0)
     (with-data (p vector-double 4)
       (setf (data p) #(0.1d0 0.2d0 0.3d0 0.4d0))
       (with-data (n vector-fixnum 4)
	 (multinomial *rng-mt19937* 8 p n)
	 (data n)))))
  (lisp-unit:assert-first-fp-equal
   "0.806400000000d-04"
   (with-data (p vector-double 4)
     (setf (data p) #(0.1d0 0.2d0 0.3d0 0.4d0))
     (with-data (n vector-fixnum 4)
       (setf (data n) #(5 0 1 2))
       (multinomial-pdf p N))))
  (lisp-unit:assert-first-fp-equal
   "-0.942551575364d+01"
   (with-data (p vector-double 4)
     (setf (data p) #(0.1d0 0.2d0 0.3d0 0.4d0))
     (with-data (n vector-fixnum 4)
       (setf (data n) #(5 0 1 2))
       (multinomial-log-pdf p n)))))
