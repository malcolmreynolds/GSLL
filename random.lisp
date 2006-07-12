;********************************************************
; file:        random.lisp                               
; description: Random number generation                  
; date:        Tue Jul 11 2006 - 23:39                   
; author:      Liam M. Healy                             
; modified:    Wed Jul 12 2006 - 00:54
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl setup-random-number-generator ()
  "gsl_rng_env_setup" ()
  :c-return :pointer
  :documentation "Setup random number generator.")

;;;;****************************************************************************
;;;; Random number generator types
;;;;****************************************************************************

(cffi:defcstruct random-number-generator-type
  (name :pointer)
  (max :unsigned-long)
  (min :unsigned-long)
  (size :size)
  (set :pointer)
  (get :pointer)
  (get-double :pointer))

(defmacro def-rng-constant (gsl-name &optional documentation lisp-name)
  "Define the random number generator type."
  (let ((lname
	 (or lisp-name
	     (intern
	      (format nil "*~:@(~a~)*"
		      (substitute #\- #\_ (subseq gsl-name 8)))))))
    `(progn (cffi:defcvar (,gsl-name ,lname)
		:pointer :read-only t)
      (setf (documentation ',lname 'variable) ,documentation))))

(def-rng-constant "gsl_rng_default"
    "The default, set by environment variables GSL_RNG_TYPE and GSL_RNG_SEED"
  *rngdefault*)

(def-rng-constant "gsl_rng_borosh13"
    "The Borosh-Niederreiter random number generator. It is taken
    from Knuth's @cite{Seminumerical Algorithms}, 3rd Ed., pages
    106--108. Its sequence is x_{n+1} = (a x_n) mod m
    with @math{a = 1812433253} and @math{m = 2^32}.
    The seed specifies the initial value, @math{x_1}.")

(def-rng-constant "gsl_rng_coveyou")
(def-rng-constant "gsl_rng_cmrg")
(def-rng-constant "gsl_rng_fishman18")
(def-rng-constant "gsl_rng_fishman20")
(def-rng-constant "gsl_rng_fishman2x")
(def-rng-constant "gsl_rng_gfsr4")
(def-rng-constant "gsl_rng_knuthran")
(def-rng-constant "gsl_rng_knuthran2")
(def-rng-constant "gsl_rng_lecuyer21")
(def-rng-constant "gsl_rng_minstd")
(def-rng-constant "gsl_rng_mrg")
(def-rng-constant "gsl_rng_mt19937")
(def-rng-constant "gsl_rng_mt19937_1999")
(def-rng-constant "gsl_rng_mt19937_1998")
(def-rng-constant "gsl_rng_r250")
(def-rng-constant "gsl_rng_ran0")
(def-rng-constant "gsl_rng_ran1")
(def-rng-constant "gsl_rng_ran2")
(def-rng-constant "gsl_rng_ran3")
(def-rng-constant "gsl_rng_rand")
(def-rng-constant "gsl_rng_rand48")
(def-rng-constant "gsl_rng_random128_bsd")
(def-rng-constant "gsl_rng_random128_glibc2")
(def-rng-constant "gsl_rng_random128_libc5")
(def-rng-constant "gsl_rng_random256_bsd")
(def-rng-constant "gsl_rng_random256_glibc2")
(def-rng-constant "gsl_rng_random256_libc5")
(def-rng-constant "gsl_rng_random32_bsd")
(def-rng-constant "gsl_rng_random32_glibc2")
(def-rng-constant "gsl_rng_random32_libc5")
(def-rng-constant "gsl_rng_random64_bsd")
(def-rng-constant "gsl_rng_random64_glibc2")
(def-rng-constant "gsl_rng_random64_libc5")
(def-rng-constant "gsl_rng_random8_bsd")
(def-rng-constant "gsl_rng_random8_glibc2")
(def-rng-constant "gsl_rng_random8_libc5")
(def-rng-constant "gsl_rng_random_bsd")
(def-rng-constant "gsl_rng_random_glibc2")
(def-rng-constant "gsl_rng_random_libc5")
(def-rng-constant "gsl_rng_randu")
(def-rng-constant "gsl_rng_ranf")
(def-rng-constant "gsl_rng_ranlux")
(def-rng-constant "gsl_rng_ranlux389")
(def-rng-constant "gsl_rng_ranlxd1")
(def-rng-constant "gsl_rng_ranlxd2")
(def-rng-constant "gsl_rng_ranlxs0")
(def-rng-constant "gsl_rng_ranlxs1")
(def-rng-constant "gsl_rng_ranlxs2")
(def-rng-constant "gsl_rng_ranmar")
(def-rng-constant "gsl_rng_slatec")
(def-rng-constant "gsl_rng_taus")
(def-rng-constant "gsl_rng_taus2")
(def-rng-constant "gsl_rng_taus113")
(def-rng-constant "gsl_rng_transputer")
(def-rng-constant "gsl_rng_tt800")
(def-rng-constant "gsl_rng_uni")
(def-rng-constant "gsl_rng_uni32")
(def-rng-constant "gsl_rng_vax")
(def-rng-constant "gsl_rng_waterman14")
(def-rng-constant "gsl_rng_zuf")
