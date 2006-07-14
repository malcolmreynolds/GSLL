;********************************************************
; file:        random.lisp                               
; description: Random number generation                  
; date:        Tue Jul 11 2006 - 23:39                   
; author:      Liam M. Healy                             
; modified:    Thu Jul 13 2006 - 22:59
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

(defun-gsl rng-name (rng)
  "gsl_rng_name" ((rng :pointer))
  :c-return :string
  :documentation "The name of the random number generator.")

(defun-gsl rng-max (rng)
  "gsl_rng_max" ((rng :pointer))
  :c-return :unsigned-long
  :documentation "The largest value that @code{gsl_rng_get}
   can return.")

(defun-gsl rng-min (rng)
  "gsl_rng_min" ((rng :pointer))
  :c-return :unsigned-long
  :documentation "The smallest value that @code{gsl_rng_get}
   can return.  Usually this value is zero.  There are some generators with
   algorithms that cannot return zero, and for these generators the minimum
   value is 1.")

;;;;****************************************************************************
;;;; Defining RNGs and default
;;;;****************************************************************************

(defmacro def-rng-type (lisp-name &optional documentation gsl-name)
  "Define the random number generator type."
  (let ((cname
	 (or gsl-name
	     (remove #\*
		     (substitute #\_ #\- 
				 (format nil "gsl_rng_~(~a~)" lisp-name)))))
	(sym (gensym "RNG")))
    `(progn
      (cffi:defcvar (,cname ,sym) :pointer :read-only t)
      (defparameter ,lisp-name (cffi:get-var-pointer ',sym)
	,documentation)
      (map-name ',lisp-name ,cname))))

(def-rng-type *rngdefault*
    "The default, set by environment variables GSL_RNG_TYPE and GSL_RNG_SEED"
  "gsl_rng_default")

;;;;****************************************************************************
;;;; Modern random number generators
;;;;****************************************************************************

(def-rng-type *cmrg*
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

(def-rng-type *gfsr4*
    "Four-tap Generalized Feedback Shift Register
    The @code{gfsr4} generator is like a lagged-fibonacci generator, and 
    produces each number as an @code{xor}'d sum of four previous values.
    r_n = r_{n-A} \oplus r_{n-B} \oplus r_{n-C} \oplus r_{n-D}
    Ziff (ref below) notes that ``it is now widely known'' that two-tap
    registers (such as R250, which is described below)
    have serious flaws, the most obvious one being the three-point
    correlation that comes from the definition of the generator.  Nice
    mathematical properties can be derived for GFSR's, and numerics bears
    out the claim that 4-tap GFSR's with appropriately chosen offsets are as
    random as can be measured, using the author's test.

    This implementation uses the values suggested the example on p392 of
    Ziff's article: @math{A=471}, @math{B=1586}, @math{C=6988}, @math{D=9689}.

    If the offsets are appropriately chosen (such as the one ones in this
    implementation), then the sequence is said to be maximal; that means
    that the period is @math{2^D - 1}, where @math{D} is the longest lag.
    (It is one less than @math{2^D} because it is not permitted to have all
    zeros in the @code{ra[]} array.)  For this implementation with
    @math{D=9689} that works out to about @math{10^2917}.

    Note that the implementation of this generator using a 32-bit
    integer amounts to 32 parallel implementations of one-bit
    generators.  One consequence of this is that the period of this
    32-bit generator is the same as for the one-bit generator.
    Moreover, this independence means that all 32-bit patterns are
    equally likely, and in particular that 0 is an allowed random
    value.  (We are grateful to Heiko Bauke for clarifying for us these
    properties of GFSR random number generators.)

    For more information see,
    Robert M. Ziff, ``Four-tap shift-register-sequence random-number 
    generators'', @cite{Computers in Physics}, 12(4), Jul/Aug
    1998, pp 385--392.")

(def-rng-type *mrg*
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

(def-rng-type *mt19937*
    "The MT19937 generator of Makoto Matsumoto and Takuji Nishimura is a
    variant of the twisted generalized feedback shift-register algorithm,
    and is known as the ``Mersenne Twister'' generator.  It has a Mersenne
    prime period of @math{2^19937 - 1} (about @math{10^6000}) and is
    equi-distributed in 623 dimensions.  It has passed the @sc{diehard}
    statistical tests.  It uses 624 words of state per generator and is
    comparable in speed to the other generators.  The original generator used
    a default seed of 4357 and choosing @var{s} equal to zero in
    @code{gsl_rng_set} reproduces this.
    For more information see,
    Makoto Matsumoto and Takuji Nishimura, ``Mersenne Twister: A
    623-dimensionally equidistributed uniform pseudorandom number
    generator''. @cite{ACM Transactions on Modeling and Computer
    Simulation}, Vol.@: 8, No.@: 1 (Jan. 1998), Pages 3--30
    The generator @code{gsl_rng_mt19937} uses the second revision of the
    seeding procedure published by the two authors above in 2002.  The
    original seeding procedures could cause spurious artifacts for some seed
    values. They are still available through the alternative generators")
									 
(def-rng-type *mt19937_1999* "Previous version of mt19937.")

(def-rng-type *mt19937_1998* "Previous version of mt19937.")

(def-rng-type *ran0*)

(def-rng-type *ran1*)

(def-rng-type *ran2*)

(def-rng-type *ran3*)

(def-rng-type *ranlux*
    "The @code{ranlux} generator is an implementation of the original
    algorithm developed by Lüscher.  It uses a
    lagged-fibonacci-with-skipping algorithm to produce ``luxury random
    numbers''.  It is a 24-bit generator, originally designed for
    single-precision IEEE floating point numbers.  This implementation is
    based on integer arithmetic, while the second-generation versions
    @sc{ranlxs} and @sc{ranlxd} described above provide floating-point
    implementations which will be faster on many platforms.
    The period of the generator is about @c{$10^{171}$} 
    @math{10^171}.  The algorithm has mathematically proven properties and
    it can provide truly decorrelated numbers at a known level of
    randomness.  The default level of decorrelation recommended by Lüscher
    is provided by @code{gsl_rng_ranlux}, while @code{gsl_rng_ranlux389}
    gives the highest level of randomness, with all 24 bits decorrelated.
    Both types of generator use 24 words of state per generator.
    For more information see,
    M. Lüscher, ``A portable high-quality random number generator for
    lattice field theory calculations'', @cite{Computer Physics
    Communications}, 79 (1994) 100--110.
    F. James, ``RANLUX: A Fortran implementation of the high-quality
    pseudo-random number generator of Lüscher'', @cite{Computer Physics
    Communications}, 79 (1994) 111--114.")

(def-rng-type *ranlux389*)

(def-rng-type *ranlxs0*
    "The generator @code{ranlxs0} is a second-generation version of the
    @sc{ranlux} algorithm of Lüscher, which produces ``luxury random
    numbers''.  This generator provides single precision output (24 bits) at
    three luxury levels @code{ranlxs0}, @code{ranlxs1} and @code{ranlxs2}.
    It uses double-precision floating point arithmetic internally and can be
    significantly faster than the integer version of @code{ranlux},
    particularly on 64-bit architectures.  The period of the generator is
    about @c{$10^{171}$} 
    @math{10^171}.  The algorithm has mathematically proven properties and
    can provide truly decorrelated numbers at a known level of randomness.
    The higher luxury levels provide increased decorrelation between samples
    as an additional safety margin.")

(def-rng-type *ranlxs1*)

(def-rng-type *ranlxs2*)

(def-rng-type *ranlxd1*
    "These generators produce double precision output (48 bits) from the
    @sc{ranlxs} generator.  The library provides two luxury levels
    @code{ranlxd1} and @code{ranlxd2}.")

(def-rng-type *ranlxd2*)

(def-rng-type *taus*
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

(def-rng-type *taus2*
    "The same algorithm as *taus* but with an improved seeding procedure
     described in the paper,
     P. L'Ecuyer, ``Tables of Maximally Equidistributed Combined LFSR
     Generators'', @cite{Mathematics of Computation}, 68, 225 (1999), 261--269
     The generator *taus2* should now be used in preference to *taus*.")

(def-rng-type *taus113*)

;;;;****************************************************************************
;;;; UNIX random number generators
;;;;****************************************************************************

(def-rng-type *rand*
    "The BSD @code{rand()} generator.  Its sequence is
   x_{n+1} = (a x_n + c) mod m with @math{a = 1103515245}, 
   @math{c = 12345} and  @math{m = 2^31}.
   The seed specifies the initial value,  @math{x_1}.
   The period of this generator is @math{2^31}, and it uses
   1 word of storage per generator.")

(def-rng-type *rand48*
    "The Unix @code{rand48} generator.  Its sequence is
     x_{n+1} = (a x_n + c) mod m
     defined on 48-bit unsigned integers with 
     @math{a = 25214903917}, @math{c = 11} and @math{m = 2^48}. 
     The seed specifies the upper 32 bits of the initial value, @math{x_1},
     with the lower 16 bits set to @code{0x330E}.  The function
     @code{gsl_rng_get} returns the upper 32 bits from each term of the
     sequence.  This does not have a direct parallel in the original
     @code{rand48} functions, but forcing the result to type @code{long int}
     reproduces the output of @code{mrand48}.  The function
     @code{gsl_rng_uniform} uses the full 48 bits of internal state to return
     the double precision number @math{x_n/m}, which is equivalent to the
     function @code{drand48}.  Note that some versions of the GNU C Library
     contained a bug in @code{mrand48} function which caused it to produce
     different results (only the lower 16-bits of the return value were set).")

(def-rng-type *random128_bsd*)
(def-rng-type *random128_glibc2*)
(def-rng-type *random128_libc5*)
(def-rng-type *random256_bsd*)
(def-rng-type *random256_glibc2*)
(def-rng-type *random256_libc5*)
(def-rng-type *random32_bsd*)
(def-rng-type *random32_glibc2*)
(def-rng-type *random32_libc5*)
(def-rng-type *random64_bsd*)
(def-rng-type *random64_glibc2*)
(def-rng-type *random64_libc5*)
(def-rng-type *random8_bsd*)
(def-rng-type *random8_glibc2*)
(def-rng-type *random8_libc5*)
(def-rng-type *random_bsd*)
(def-rng-type *random_glibc2*)
(def-rng-type *random_libc5*)

;;;;****************************************************************************
;;;; Obsolete random number generators
;;;;****************************************************************************

;;; Other random number generators

;;; The generators in this section are provided for compatibility with
;;; existing libraries.  If you are converting an existing program to use GSL
;;; then you can select these generators to check your new implementation
;;; against the original one, using the same random number generator.  After
;;; verifying that your new program reproduces the original results you can
;;; then switch to a higher-quality generator.

;;; Note that most of the generators in this section are based on single
;;; linear congruence relations, which are the least sophisticated type of
;;; generator.  In particular, linear congruences have poor properties when
;;; used with a non-prime modulus, as several of these routines do (e.g.
;;; with a power of two modulus, 
;;; @c{$2^{31}$}
;;; @math{2^31} or 
;;; @c{$2^{32}$}
;;; @math{2^32}).  This
;;; leads to periodicity in the least significant bits of each number,
;;; with only the higher bits having any randomness.  Thus if you want to
;;; produce a random bitstream it is best to avoid using the least
;;; significant bits.

(def-rng-type *ranf* "Obsolete, use only for compatibility.")

(def-rng-type *ranmar* "Obsolete, use only for compatibility.")

(def-rng-type *r250*
    "Obsolete, use only for compatibility.
     The shift-register generator of Kirkpatrick and Stoll.  The
     sequence is based on the recurrence
     x_n = x_{n-103} \oplus x_{n-250} where  @c{$\oplus$}
     denotes ``exclusive-or'', defined on 32-bit words.
     The period of this generator is about @math{2^250} and it
     uses 250 words of state per generator.
     For more information see,
     S. Kirkpatrick and E. Stoll, ``A very fast shift-register sequence random
     number generator'', @cite{Journal of Computational Physics}, 40, 517--526
     (1981)")

(def-rng-type *tt800*
    "Obsolete, use only for compatibility.
     An earlier version of the twisted generalized feedback
     shift-register generator, and has been superseded by the development of
     MT19937.  However, it is still an acceptable generator in its own
     right.  It has a period of @math{2^800} and uses 33 words of storage
     per generator.
     For more information see,
     Makoto Matsumoto and Yoshiharu Kurita, ``Twisted GFSR Generators
     II'', @cite{ACM Transactions on Modelling and Computer Simulation},
     Vol.: 4, No.: 3, 1994, pages 254--266.")

;;; The following generators are included only for historical reasons, so
;;; that you can reproduce results from old programs which might have used
;;; them.  These generators should not be used for real simulations since
;;; they have poor statistical properties by modern standards.

(def-rng-type *vax* "Obsolete, use only for compatibility.")

(def-rng-type *transputer* "Obsolete, use only for compatibility.")

(def-rng-type *randu* "Obsolete, use only for compatibility.")

(def-rng-type *minstd*
    "Obsolete, use only for compatibility.
    Park and Miller's ``minimal standard'' @sc{minstd} generator, a
    simple linear congruence which takes care to avoid the major pitfalls of
    such algorithms.  Its sequence is x_@{n+1@} = (a x_n) mod m
    with @math{a = 16807} and @math{m = 2^31 - 1 = 2147483647}. 
    The seed specifies the initial value, @math{x_1}.  The period of this
    generator is about @math{2^31}.
    This generator is used in the IMSL Library (subroutine RNUN) and in
    MATLAB (the RAND function).  It is also sometimes known by the acronym
    ``GGL'' (I'm not sure what that stands for).
    For more information see
    Park and Miller, ``Random Number Generators: Good ones are hard to find'',
    @cite{Communications of the ACM}, October 1988, Volume 31, No 10, pages
    1192--1201.")

(def-rng-type *uni* "Obsolete, use only for compatibility.")

(def-rng-type *uni32* "Obsolete, use only for compatibility.")

(def-rng-type *slatec* "Obsolete, use only for compatibility.")

(def-rng-type *zuf*
    "Obsolete, use only for compatibility.
    The ZUFALL lagged Fibonacci series generator of Peterson.  Its
    sequence is
        t = u_@{n-273@} + u_@{n-607@}
        u_n  = t - floor(t)
    The original source code is available from NETLIB.  For more information
    see
    W. Petersen, ``Lagged Fibonacci Random Number Generators for the NEC
    SX-3'', @cite{International Journal of High Speed Computing} (1994).")

(def-rng-type *borosh13*
    "Obsolete, use only for compatibility.
    The Borosh-Niederreiter random number generator. It is taken
    from Knuth's @cite{Seminumerical Algorithms}, 3rd Ed., pages
    106--108. Its sequence is x_{n+1} = (a x_n) mod m
    with @math{a = 1812433253} and @math{m = 2^32}.
    The seed specifies the initial value, @math{x_1}.")

(def-rng-type *coveyou*
    "Obsolete, use only for compatibility.
     The Coveyou random number generator, taken from Knuth's
     @cite{Seminumerical Algorithms}, 3rd Ed., Section 3.2.2. Its sequence
     is x_@{n+1@} = (x_n (x_n + 1)) mod m with @math{m = 2^32}.
     The seed specifies the initial value, @math{x_1}.")

(def-rng-type *fishman18*
    "Obsolete, use only for compatibility.
     The Fishman, Moore III random number generator. It is taken from
     Knuth's @cite{Seminumerical Algorithms}, 3rd Ed., pages 106--108. Its
     sequence is x_@{n+1@} = (a x_n) mod m with @math{a = 62089911} and 
     @math{m = 2^31 - 1}.  The seed specifies the initial value, 
     @math{x_1}.")

(def-rng-type *fishman20*
    "Obsolete, use only for compatibility.
     The Fishman random number generator. It is taken from Knuth's
     @cite{Seminumerical Algorithms}, 3rd Ed., page 108. Its sequence is
     x_@{n+1@} = (a x_n) mod m with @math{a = 48271} and 
     @math{m = 2^31 - 1}.  The seed specifies the initial value, 
     @math{x_1}.")

(def-rng-type *fishman2x*
    "Obsolete, use only for compatibility.
     The L'Ecuyer--Fishman random number generator. It is taken from
     Knuth's @cite{Seminumerical Algorithms}, 3rd Ed., page 108.
     Its sequence is z_@{n+1@} = (x_n - y_n) mod m with
     @math{m = 2^31 - 1}.
     @math{x_n} and @math{y_n} are given by the @code{fishman20} 
     and @code{lecuyer21} algorithms.
     The seed specifies the initial value, @math{x_1}.")

(def-rng-type *knuthran*
    "Obsolete, use only for compatibility.
    A second-order multiple recursive generator described by Knuth
    in @cite{Seminumerical Algorithms}, 3rd Ed., Section 3.6.  Knuth
    provides its C code.")

(def-rng-type *knuthran2*
    "Obsolete, use only for compatibility.
     A second-order multiple recursive generator described by Knuth
     in @cite{Seminumerical Algorithms}, 3rd Ed., page 108.  Its sequence is
     x_n = (a_1 x_{n-1} + a_2 x_{n-2}) \,\hbox{mod}\, m
     with @math{a_1 = 271828183}, @math{a_2 = 314159269}, and 
     @math{m = 2^31 - 1}.")

(def-rng-type *lecuyer21*
    "Obsolete, use only for compatibility.
     The L'Ecuyer random number generator, taken from Knuth's
     @cite{Seminumerical Algorithms}, 3rd Ed., page 106--108.
     Its sequence is x_@{n+1@} = (a x_n) mod m
     with @math{a = 40692} and @math{m = 2^31 - 249}.
     The seed specifies the initial value, @math{x_1}.")

(def-rng-type *waterman14*
    "Obsolete, use only for compatibility.
     The Waterman random number generator. It is taken from Knuth's
     @cite{Seminumerical Algorithms}, 3rd Ed., page 106--108.
     Its sequence is x_@{n+1@} = (a x_n) mod m with
     @math{a = 1566083941} and @math{m = 2^32}.
     The seed specifies the initial value, @math{x_1}.")
