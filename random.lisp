;********************************************************
; file:        random.lisp                               
; description: Random number generation                  
; date:        Tue Jul 11 2006 - 23:39                   
; author:      Liam M. Healy                             
; modified:    Wed Jul 12 2006 - 23:04
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

;;; needed?
(cffi:defcstruct random-number-generator-type
  (name :pointer)
  (max :unsigned-long)
  (min :unsigned-long)
  (size :size)
  (set :pointer)
  (get :pointer)
  (get-double :pointer))

;;;;****************************************************************************
;;;; Auxiliary functions
;;;;****************************************************************************

;; memory fault?
(defun-gsl rng-name (rng)
  "gsl_rng_name" ((rng :pointer))
  :c-return (cret :pointer)
  :return ((cffi:foreign-string-to-lisp cret))
  :documentation "The name of the random number generator.")

;;; (foreign-funcall "getenv" :string "HOME" :string)
;;; works, but this gets a memory fault.
;;; const char *gsl_rng_name (const gsl_rng * r);
(defun-gsl rng-name (rng)
  "gsl_rng_name" ((rng :pointer))
  :c-return (cret :string)
  :documentation "The name of the random number generator.")


(defun-gsl rng-max (rng)
  "gsl_rng_max" ((rng :pointer))
  :c-return :unsigned-long
  :documentation "The largest value that @code{gsl_rng_get}
   can return.")

;; frequently gibberish? (rng-min *coveyou*) => 1598837575
(defun-gsl rng-min (rng)
  "gsl_rng_min" ((rng :pointer))
  :c-return :unsigned-long
  :documentation "The smallest value that @code{gsl_rng_get}
   can return.  Usually this value is zero.  There are some generators with
   algorithms that cannot return zero, and for these generators the minimum
   value is 1.")

;;;;****************************************************************************
;;;; Random number generator algorithms
;;;;****************************************************************************

(defmacro def-rng-constant (lisp-name &optional documentation gsl-name)
  "Define the random number generator type."
  (let ((cname
	 (or gsl-name
	     (remove #\*
		     (substitute #\_ #\- 
				 (format nil "gsl_rng_~(~a~)" lisp-name))))))
    `(progn (cffi:defcvar (,cname ,lisp-name)
		:pointer :read-only t)
	    (setf (documentation ',lisp-name 'variable) ,documentation)
	    (map-name ',lisp-name ,cname))))

(def-rng-constant *rngdefault*
    "The default, set by environment variables GSL_RNG_TYPE and GSL_RNG_SEED"
  "gsl_rng_default")

(def-rng-constant *borosh13*
    "The Borosh-Niederreiter random number generator. It is taken
    from Knuth's @cite{Seminumerical Algorithms}, 3rd Ed., pages
    106--108. Its sequence is x_{n+1} = (a x_n) mod m
    with @math{a = 1812433253} and @math{m = 2^32}.
    The seed specifies the initial value, @math{x_1}.")

(def-rng-constant *coveyou*
    "The Coveyou random number generator, taken from Knuth's
     @cite{Seminumerical Algorithms}, 3rd Ed., Section 3.2.2. Its sequence
     is x_@{n+1@} = (x_n (x_n + 1)) mod m with @math{m = 2^32}.
     The seed specifies the initial value, @math{x_1}.")

(def-rng-constant *cmrg*
    "Combined multiple recursive random number generator
     This is a combined multiple recursive generator by L'Ecuyer. 
     Its sequence is z_n = (x_n - y_n) mod m_1
     where the two underlying generators @math{x_n} and @math{y_n} are,
     x_n = (a_1 x_@{n-1@} + a_2 x_@{n-2@} + a_3 x_@{n-3@}) mod m_1
     y_n = (b_1 y_@{n-1@} + b_2 y_@{n-2@} + b_3 y_@{n-3@}) mod m_2
     with coefficients 
     @math{a_1 = 0}, 
     @math{a_2 = 63308}, 
     @math{a_3 = -183326},
     @math{b_1 = 86098}, 
     @math{b_2 = 0},
     @math{b_3 = -539608},
     and moduli 
     @math{m_1 = 2^31 - 1 = 2147483647}
     and 
     @math{m_2 = 2145483479}.
    The period of this generator is 
    @math{2^205} 
    (about 
    @c{$10^{61}$}
    @math{10^61}).  It uses
    6 words of state per generator.  For more information see,
    P. L'Ecuyer, ``Combined Multiple Recursive Random Number
    Generators'', @cite{Operations Research}, 44, 5 (1996), 816--822.")

(def-rng-constant *fishman18*
    "The Fishman, Moore III random number generator. It is taken from
     Knuth's @cite{Seminumerical Algorithms}, 3rd Ed., pages 106--108. Its
     sequence is x_@{n+1@} = (a x_n) mod m with @math{a = 62089911} and 
     @math{m = 2^31 - 1}.  The seed specifies the initial value, 
     @math{x_1}.")

(def-rng-constant *fishman20*
    "The Fishman random number generator. It is taken from Knuth's
     @cite{Seminumerical Algorithms}, 3rd Ed., page 108. Its sequence is
     x_@{n+1@} = (a x_n) mod m with @math{a = 48271} and 
     @math{m = 2^31 - 1}.  The seed specifies the initial value, 
     @math{x_1}.")

(def-rng-constant *fishman2x*)
(def-rng-constant *gfsr4*)
(def-rng-constant *knuthran*)
(def-rng-constant *knuthran2*)
(def-rng-constant *lecuyer21*)
(def-rng-constant *minstd*)

(def-rng-constant *mrg*
    "Multiple recursive random number generator
   This is a fifth-order multiple recursive generator by L'Ecuyer, Blouin
   and Coutre.  Its sequence is
   x_n = (a_1 x_@{n-1@} + a_5 x_@{n-5@}) mod m
   with 
    @math{a_1 = 107374182}, 
    @math{a_2 = a_3 = a_4 = 0}, 
    @math{a_5 = 104480}
    and 
    @math{m = 2^31 - 1}.
   The period of this generator is about  @math{10^46}.  It uses 5 words
   of state per generator.  More information can be found in the following
   paper,
    P. L'Ecuyer, F. Blouin, and R. Coutre, ``A search for good multiple
    recursive random number generators'', @cite{ACM Transactions on Modeling and
    Computer Simulation} 3, 87--98 (1993).")


(def-rng-constant *mt19937*)
(def-rng-constant *mt19937_1999*)
(def-rng-constant *mt19937_1998*)
(def-rng-constant *r250*)
(def-rng-constant *ran0*)
(def-rng-constant *ran1*)
(def-rng-constant *ran2*)
(def-rng-constant *ran3*)
(def-rng-constant *rand*)
(def-rng-constant *rand48*)
(def-rng-constant *random128_bsd*)
(def-rng-constant *random128_glibc2*)
(def-rng-constant *random128_libc5*)
(def-rng-constant *random256_bsd*)
(def-rng-constant *random256_glibc2*)
(def-rng-constant *random256_libc5*)
(def-rng-constant *random32_bsd*)
(def-rng-constant *random32_glibc2*)
(def-rng-constant *random32_libc5*)
(def-rng-constant *random64_bsd*)
(def-rng-constant *random64_glibc2*)
(def-rng-constant *random64_libc5*)
(def-rng-constant *random8_bsd*)
(def-rng-constant *random8_glibc2*)
(def-rng-constant *random8_libc5*)
(def-rng-constant *random_bsd*)
(def-rng-constant *random_glibc2*)
(def-rng-constant *random_libc5*)
(def-rng-constant *randu*)
(def-rng-constant *ranf*)
(def-rng-constant *ranlux*)
(def-rng-constant *ranlux389*)
(def-rng-constant *ranlxd1*)
(def-rng-constant *ranlxd2*)
(def-rng-constant *ranlxs0*)
(def-rng-constant *ranlxs1*)
(def-rng-constant *ranlxs2*)
(def-rng-constant *ranmar*)
(def-rng-constant *slatec*)

(def-rng-constant *taus*
    "Tausworthe random number generator
     This is a maximally equidistributed combined Tausworthe generator by
     L'Ecuyer.  The sequence is x_n = (s^1_n \oplus s^2_n \oplus s^3_n) 
     \eqalign{
     s^1_{n+1} &= (((s^1_n \& 4294967294)\ll 12)
     \oplus (((s^1_n\ll 13) \oplus s^1_n)\gg 19)) \cr
     s^2_{n+1} &= (((s^2_n \& 4294967288)\ll 4)
     \oplus (((s^2_n\ll 2) \oplus s^2_n)\gg 25)) \cr
     s^3_{n+1} &= (((s^3_n \& 4294967280)\ll 17)
     \oplus (((s^3_n\ll 3) \oplus s^3_n)\gg 11))
     }
     s1_@{n+1@} = (((s1_n&4294967294)<<12)^^(((s1_n<<13)^^s1_n)>>19))
     s2_@{n+1@} = (((s2_n&4294967288)<< 4)^^(((s2_n<< 2)^^s2_n)>>25))
     s3_@{n+1@} = (((s3_n&4294967280)<<17)^^(((s3_n<< 3)^^s3_n)>>11))
     computed modulo @math{2^32}.  In the formulas above @c{$\oplus$}
     denotes ``exclusive-or''.  Note that the algorithm relies on the properties
     of 32-bit unsigned integers and has been implemented using a bitmask
     of @code{0xFFFFFFFF} to make it work on 64 bit machines.
     The period of this generator is @math{2^88} (about
     @c{$10^{26}$}
     @math{10^26}).  It uses 3 words of state per generator.  For more
     information see,
     P. L'Ecuyer, ``Maximally Equidistributed Combined Tausworthe
     Generators'', @cite{Mathematics of Computation}, 65, 213 (1996), 203--213.")

(def-rng-constant *taus2*
    "The same algorithm as *taus* but with an improved seeding procedure
     described in the paper,
     P. L'Ecuyer, ``Tables of Maximally Equidistributed Combined LFSR
     Generators'', @cite{Mathematics of Computation}, 68, 225 (1999), 261--269
     The generator *taus2* should now be used in preference to *taus*.")

(def-rng-constant *taus113*)
(def-rng-constant *transputer*)
(def-rng-constant *tt800*)
(def-rng-constant *uni*)
(def-rng-constant *uni32*)
(def-rng-constant *vax*)
(def-rng-constant *waterman14*)
(def-rng-constant *zuf*)
