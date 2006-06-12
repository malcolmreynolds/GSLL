;********************************************************
; file:        bessel.lisp                               
; description: Bessel functions                          
; date:        Fri Mar 17 2006 - 18:42                   
; author:      Liam M. Healy
; modified:    Sun Jun 11 2006 - 21:01
;********************************************************

(in-package :gsl)
;;; with-data crashes sbcl


;;;;****************************************************************************
;;;; Regular Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-gsl cylindrical-bessel-J0 (x)
    "gsl_sf_bessel_J0_e"
  ((x :double) (ret sf-result))
  :documentation
  "The regular cylindrical Bessel function of zeroth order, @math{J_0(x)}.")

(defun-gsl cylindrical-bessel-J1 (x)
  "gsl_sf_bessel_J1_e" ((x :double) (ret sf-result))
  :documentation
  "The regular cylindrical Bessel function of first order, @math{J_1(x)}.")

(defun-gsl cylindrical-bessel-Jn (n x)
  "gsl_sf_bessel_Jn_e" ((n :int) (x :double) (ret sf-result))
  :documentation
  "The regular cylindrical Bessel function of order @var{n}, @math{J_n(x)}.")

(defun-gsl cylindrical-bessel-Jn-array (x array &optional (nmin 0))
  "gsl_sf_bessel_Jn_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :documentation
  "The values of the regular cylindrical Bessel functions @math{J_n(x)}
   for @math{n} from @var{nmin} to nmin+length(array)-1 inclusive.
   The values are computed using recurrence relations for efficiency,
   and therefore may differ slightly from the exact values."
  :invalidate (array))

;;;;****************************************************************************
;;;; Irregular Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-gsl cylindrical-bessel-Y0 (x)
  "gsl_sf_bessel_Y0_e" ((x :double) (ret sf-result))
  :documentation
  "The irregular cylindrical Bessel function of zeroth order, @math{Y_0(x)}.")

(defun-gsl cylindrical-bessel-Y1 (x)
  "gsl_sf_bessel_Y1_e" ((x :double) (ret sf-result))
  :documentation
  "The irregular cylindrical Bessel function of first order, @math{Y_1(x)}.")

(defun-gsl cylindrical-bessel-Yn (n x)
  "gsl_sf_bessel_Yn_e"
  ((n :int) (x :double) (ret sf-result))
  :documentation
  "The irregular cylindrical Bessel function of order @var{n}, @math{Y_n(x)}.")

(defun-gsl cylindrical-bessel-Yn-array (x array &optional (nmin 0))
  "gsl_sf_bessel_Yn_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :documentation
  "The values of the irregular cylindrical Bessel functions
   @math{Y_n(x)} for @math{n} from from @var{nmin} to
   nmin+length(array)-1 inclusive.  The values are computed using
   recurrence relations for efficiency, and therefore may differ slightly
   from the exact values."
  :invalidate (array))

;;;;****************************************************************************
;;;; Regular Modified Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-gsl cylindrical-bessel-I0 (x)
  "gsl_sf_bessel_I0_e" ((x :double) (ret sf-result))
  :documentation
  "The regular modified cylindrical Bessel function of zeroth order, @math{I_0(x)}.")

(defun-gsl cylindrical-bessel-I1 (x)
  "gsl_sf_bessel_I1_e" ((x :double) (ret sf-result))
  :documentation
  "The regular modified cylindrical Bessel function of first order, @math{I_1(x)}.")

(defun-gsl cylindrical-bessel-In (n x)
  "gsl_sf_bessel_In_e" ((n :int) (x :double) (ret sf-result))
  :documentation
  "The regular modified cylindrical Bessel function of order @var{n}, @math{I_n(x)}.")

(defun-gsl cylindrical-bessel-In-array (x array &optional (nmin 0))
  "gsl_sf_bessel_In_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  "The values of the regular modified cylindrical Bessel functions
   @math{I_n(x)} for @math{n} from from @var{nmin} to
   nmin+length(array)-1 inclusive.
   The values are computed using recurrence relations for efficiency, and
   therefore may differ slightly from the exact values.")

(defun-gsl cylindrical-bessel-I0-scaled (x)
  "gsl_sf_bessel_I0_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled regular modified cylindrical Bessel function of zeroth order,
  @math{\exp(-|x|) I_0(x)}.")

(defun-gsl cylindrical-bessel-I1-scaled (x)
  "gsl_sf_bessel_I1_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled regular modified cylindrical Bessel function of first order,
  @math{\exp(-|x|) I_1(x)}.")

(defun-gsl cylindrical-bessel-In-scaled (n x)
  "gsl_sf_bessel_In_scaled_e" ((n :int) (x :double) (ret sf-result))
  :documentation
  "The scaled regular modified cylindrical Bessel function of order @var{n},
  @math{\exp(-|x|) I_n(x)}}.")

(defun-gsl cylindrical-bessel-In-scaled-array (x array &optional (nmin 0))
  "gsl_sf_bessel_In_scaled_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  ;; Bug in original documentation?  doesn't say "modified"
  "The values of the scaled regular modified cylindrical Bessel
  functions @math{I_n(x)} for @math{n} from from @var{nmin} to
  nmin+length(array)-1 inclusive. The values are computed using recurrence
  relations for efficiency, and therefore may differ slightly from the
  exact values.")

;;;;****************************************************************************
;;;; Irregular Modified Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-gsl cylindrical-bessel-K0 (x)
  "gsl_sf_bessel_K0_e" ((x :double) (ret sf-result))
  :documentation
  "The irregular modified cylindrical Bessel function of zeroth order,
  @math{K_0(x)}.")

(defun-gsl cylindrical-bessel-K1 (x)
  "gsl_sf_bessel_K1_e" ((x :double) (ret sf-result))
  :documentation
  "The irregular modified cylindrical Bessel function of first order, @math{K_1(x)}.")

(defun-gsl cylindrical-bessel-Kn (n x)
  "gsl_sf_bessel_Kn_e" ((n :int) (x :double) (ret sf-result))
  :documentation
  "The irregular modified cylindrical Bessel function of order @var{n}, @math{K_n(x)}.")

(defun-gsl cylindrical-bessel-Kn-array (x array &optional (nmin 0))
  "gsl_sf_bessel_Kn_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  "The values of the irregular modified cylindrical Bessel functions
   @math{K_n(x)} for @math{n} from from @var{nmin} to
   nmin+length(array)-1 inclusive.
   The values are computed using recurrence relations for efficiency, and
   therefore may differ slightly from the exact values.")

(defun-gsl cylindrical-bessel-K0-scaled (x)
  "gsl_sf_bessel_K0_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled irregular modified cylindrical Bessel function of zeroth order,
  @math{\exp(-|x|) K_0(x)}.")

(defun-gsl cylindrical-bessel-K1-scaled (x)
  "gsl_sf_bessel_K1_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled irregular modified cylindrical Bessel function of first order,
   @math{\exp(-|x|) K_1(x)}.")

(defun-gsl cylindrical-bessel-Kn-scaled (n x)
  "gsl_sf_bessel_Kn_scaled_e" ((n :int) (x :double) (ret sf-result))
  :documentation
  "The scaled irregular modified cylindrical Bessel function of order @var{n},
  @math{\exp(-|x|) K_n(x)}}.")

(defun-gsl cylindrical-bessel-Kn-scaled-array (x array &optional (nmin 0))
  "gsl_sf_bessel_Kn_scaled_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  "The values of the scaled irregular cylindrical
   Bessel functions @math{\exp(x) K_n(x)} for @math{n} from from @var{nmin} to
   nmin+length(array)-1 inclusive, storing the results in the array
   @var{result_array}. The start of the range @var{nmin} must be positive
   or zero.  The domain of the function is @math{x>0}. The values are
   computed using recurrence relations for efficiency, and therefore
   may differ slightly from the exact values.")

;;;;****************************************************************************
;;;; Regular Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-j0 (x)
  "gsl_sf_bessel_j0_e" ((x :double) (ret sf-result))
  :documentation
  "The regular spherical Bessel function of zeroth order, @math{j_0(x)
  = \sin(x)/x}.")

(defun-gsl spherical-bessel-j1 (x)
  "gsl_sf_bessel_j1_e" ((x :double) (ret sf-result))
  :documentation
  "The regular spherical Bessel function of first order, @math{j_1(x)
   = (\sin(x)/x - \cos(x))/x}.")

(defun-gsl spherical-bessel-j2 (x)
  "gsl_sf_bessel_j2_e" ((x :double) (ret sf-result))
  :documentation
  "The regular spherical Bessel function of second order, @math{j_2(x)
   = ((3/x^2 - 1)\sin(x) - 3\cos(x)/x)/x}.")

(defun-gsl spherical-bessel-jl (l x)
  "gsl_sf_bessel_jl_e" ((l :int) (x :double) (ret sf-result))
  :documentation "The regular spherical Bessel function of order
  @var{l}, @math{j_l(x)}, for @math{l >= 0} and @math{x >= 0}.")

(defun-gsl spherical-bessel-jl-array (x array)
  "gsl_sf_bessel_jl_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  "The values of the regular spherical Bessel
  functions @math{j_l(x)} for @math{l} from 0 to length(array)-1
  and @math{x >= 0}, storing the results in the array @var{result_array}.
  The values are computed using recurrence relations for
  efficiency, and therefore may differ slightly from the exact values.")

(defun-gsl spherical-bessel-jl-steed-array (x array)
  "gsl_sf_bessel_jl_steed_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  "Uses Steed's method to compute the values of the regular spherical
  Bessel functions @math{j_l(x)} for @math{l} from 0 to length(array)-1
  inclusive for @math{x >= 0}, storing the results in the array
  @var{array}. The Steed/Barnett algorithm is described in
  @cite{Comp. Phys. Comm.} 21, 297 (1981).  Steed's method is more
  stable than the recurrence used in the other functions but is also
  slower.")

;;;;****************************************************************************
;;;; Irregular Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-y0 (x)
  "gsl_sf_bessel_y0_e" ((x :double) (ret sf-result))
  :documentation
  "The irregular spherical Bessel function of zeroth order,
  @math{y_0(x) = -\cos(x)/x}.")

(defun-gsl spherical-bessel-y1 (x)
  "gsl_sf_bessel_y1_e" ((x :double) (ret sf-result))
  :documentation
  "The irregular spherical Bessel function of first order,
  @math{y_1(x) = -(\cos(x)/x + \sin(x))/x}.")

(defun-gsl spherical-bessel-y2 (x)
  "gsl_sf_bessel_y2_e" ((x :double) (ret sf-result))
  :documentation
  "The irregular spherical Bessel function of second order,
  @math{y_2(x) = (-3/x^3 + 1/x)\cos(x) - (3/x^2)\sin(x)}.")

(defun-gsl spherical-bessel-yl (l x)
  "gsl_sf_bessel_yl_e" ((l :int) (x :double) (ret sf-result))
  :documentation
  "The irregular spherical Bessel function of order @var{l},
  @math{y_l(x)}, for @math{l >= 0}.")

(defun-gsl spherical-bessel-yl-array (x array)
  "gsl_sf_bessel_yl_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  "The irregular spherical Bessel functions @math{y_l(x)} for @math{l}
  from 0 to length(array)-1, storing the results in the array @var{array}.
  The values are computed using recurrence relations for efficiency,
  and therefore may differ slightly from the exact values.")

;;;;****************************************************************************
;;;; Regular Modified Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-i0-scaled (x)
  "gsl_sf_bessel_i0_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled regular modified spherical Bessel function of zeroth
  order, @math{\exp(-|x|) i_0(x)}.")

(defun-gsl spherical-bessel-i1-scaled (x)
  "gsl_sf_bessel_i1_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled regular modified spherical Bessel function of first order,
  @math{\exp(-|x|) i_1(x)}.")

(defun-gsl spherical-bessel-i2-scaled (x)
  "gsl_sf_bessel_i2_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled regular modified spherical Bessel function of second order,
  @math{ \exp(-|x|) i_2(x) }.")

(defun-gsl spherical-bessel-il-scaled (n x)
  "gsl_sf_bessel_il_scaled_e" ((n :int) (x :double) (ret sf-result))
  :documentation
  "The scaled regular modified spherical Bessel function of order @var{l},
  @math{ \exp(-|x|) i_l(x) }.")

(defun-gsl spherical-bessel-il-scaled-array (x array)
  "gsl_sf_bessel_il_scaled_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  "The values of the scaled regular modified cylindrical Bessel
  functions @math{\exp(-|x|) i_l(x)} for @math{l} from 0 to length(array)-1
  inclusive, storing the results in the array @var{result_array}.
  The values are computed using recurrence relations for efficiency,
  and therefore may differ slightly from the exact values.")

;;;;****************************************************************************
;;;; Irregular Modified Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-k0-scaled (x)
  "gsl_sf_bessel_k0_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled irregular modified spherical Bessel function of zeroth
  order, @math{\exp(x) k_0(x)}, for @math{x>0}.")

(defun-gsl spherical-bessel-k1-scaled (x)
  "gsl_sf_bessel_k1_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled irregular modified spherical Bessel function of first order,
   @math{\exp(x) k_1(x)}, for @math{x>0}.")

(defun-gsl spherical-bessel-k2-scaled (x)
  "gsl_sf_bessel_k2_scaled_e" ((x :double) (ret sf-result))
  :documentation
  "The scaled irregular modified spherical Bessel function of second order,
  @math{\exp(x) k_2(x)}, for @math{x>0}.")

(defun-gsl spherical-bessel-kl-scaled (n x)
  "gsl_sf_bessel_il_scaled_e" ((n :int) (x :double) (ret sf-result))
  :documentation
  "The scaled irregular modified spherical Bessel function of order @var{l},
   @math{\exp(x) k_l(x)}, for @math{x>0}.")

(defun-gsl spherical-bessel-kl-scaled-array (x array)
  "gsl_sf_bessel_kl_scaled_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation
  "The values of the scaled irregular modified spherical Bessel
  functions @math{\exp(x) k_l(x)} for @math{l} from 0 to length(array)-1
  inclusive @math{x>0}, storing the results in the array @var{result_array}.
  The values are computed using recurrence relations for efficiency,
  and therefore may differ slightly from the exact values.")

;;;;****************************************************************************
;;;; Regular Bessel Function - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Jnu (nu x)
  "gsl_sf_bessel_Jnu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation
  "The regular cylindrical Bessel function of fractional order
  @math{\nu}, @math{J_\nu(x)}.")

(defun-gsl spherical-Jnu-array (nu v)
  "gsl_sf_bessel_sequence_Jnu_e"
  ((nu :double) :mode ((dim0 v) :int) ((gsl-array v) :pointer))
  :invalidate (v)
  :documentation
  "The regular cylindrical Bessel function of
  fractional order @math{\nu}, @math{J_\nu(x)}, evaluated at a series of
  @math{x} values.  The array @var{v} contains the @math{x} values.
  They are assumed to be strictly ordered and positive.
  The array is over-written with the values of @math{J_\nu(x_i)}.")

;;;;****************************************************************************
;;;; Irregular Bessel Function - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Ynu (nu x)
  "gsl_sf_bessel_Ynu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation
  "The irregular cylindrical Bessel function of fractional order
  @math{\nu}, @math{Y_\nu(x)}.")

;;;;****************************************************************************
;;;; Regular Modified Bessel Functions - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Inu (nu x)
  "gsl_sf_bessel_Inu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation
  "The regular modified Bessel function of fractional order
  @math{\nu}, @math{I_\nu(x)} for @math{x>0}, @math{\nu>0}.")

(defun-gsl bessel-Inu-scaled (nu x)
  "gsl_sf_bessel_Inu_scaled_e" ((nu :double) (x :double) (ret sf-result))
  :documentation
  "The scaled regular modified Bessel function of fractional order
  @math{\nu}, @math{\exp(-|x|)I_\nu(x)} for @math{x>0}, @math{\nu>0}.")

;;;;****************************************************************************
;;;; Irregular Modified Bessel Functions - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Knu (nu x)
  "gsl_sf_bessel_Knu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation
  "The irregular modified Bessel function of fractional order @math{\nu},
   @math{K_\nu(x)} for @math{x>0}, @math{\nu>0}.")

(defun-gsl bessel-lnKnu (nu x)
  "gsl_sf_bessel_lnKnu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation
  "The logarithm of the irregular modified Bessel function of fractional
   order @math{\nu}, @math{\ln(K_\nu(x))} for @math{x>0}, @math{\nu>0}.")

(defun-gsl bessel-Knu-scaled (nu x)
  "gsl_sf_bessel_Knu_scaled_e" ((nu :double) (x :double) (ret sf-result))
  :documentation
  "The scaled irregular modified Bessel function of fractional order
   @math{\nu}, @math{\exp(+|x|) K_\nu(x)} for @math{x>0}, @math{\nu>0}.")

;;;;****************************************************************************
;;;; Zeros of Regular Bessel Functions
;;;;****************************************************************************

(defun-gsl bessel-zero-J0 (s)
  "gsl_sf_bessel_zero_J0_e" ((s :int) (ret sf-result))
  :documentation
  "The location of the @var{s}-th positive zero of the Bessel function
  @math{J_0(x)}.")

(defun-gsl bessel-zero-J1 (s)
  "gsl_sf_bessel_zero_J1_e" ((s :int) (ret sf-result))
  :documentation
  "The location of the @var{s}-th positive zero of the Bessel function
  @math{J_1(x)}.")

(defun-gsl bessel-zero-Jnu (nu s)
  "gsl_sf_bessel_zero_Jnu_e" ((nu :double) (s :int) (ret sf-result))
  :documentation
  "These routines compute the location of the @var{s}-th positive zero
  of the Bessel function @math{J_\nu(x)}.  The current implementation
  does not support negative values of @var{nu}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test bessel
  (lisp-unit:assert-first-fp-equal
   "-0.397149809864d+00"
   (cylindrical-bessel-J0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "-0.660433280235d-01"
   (cylindrical-bessel-J1 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.364128145852d+00"
   (cylindrical-bessel-Jn 2 4.0d0))
  (lisp-unit:assert-equal
   '("0.352834028616d+00" "0.128943249474d+00" "0.339957198076d-01"
     "0.703962975587d-02")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (cylindrical-bessel-Jn-array 2.0d0 besarr 2)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "-0.169407393251d-01"
   (cylindrical-bessel-Y0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.397925710557d+00"
   (cylindrical-bessel-Y1 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "-0.182022115953d+00"
   (cylindrical-bessel-Yn 3 4.0d0))
  (lisp-unit:assert-equal
   '("-0.617408104191d+00" "-0.112778377684d+01" "-0.276594322633d+01"
     "-0.993598912848d+01")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (cylindrical-bessel-Yn-array 2.0d0 besarr 2)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "0.113019219521d+02"
   (cylindrical-bessel-I0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.975946515370d+01"
   (cylindrical-bessel-I1 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.333727577842d+01"
   (cylindrical-bessel-In 3 4.0d0))
  (lisp-unit:assert-equal
   '("0.688948447699d+00" "0.212739959240d+00" "0.507285699792d-01"
     "0.982567932313d-02")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (cylindrical-bessel-In-array 2.0d0 besarr 2)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "0.207001921224d+00"
   (cylindrical-bessel-I0-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.178750839502d+00"
   (cylindrical-bessel-I1-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.611243380297d-01"
   (cylindrical-bessel-In-scaled 3 4.0d0))
  (lisp-unit:assert-equal
   '("0.932390333047d-01" "0.287912226395d-01" "0.686536538632d-02"
     "0.132976109419d-02")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (cylindrical-bessel-In-scaled-array 2.0d0 besarr 2)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "0.111596760859d-01"
   (cylindrical-bessel-K0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.124834988873d-01"
   (cylindrical-bessel-K1 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.174014255295d-01"
   (cylindrical-bessel-Kn 2 4.0d0))
  (lisp-unit:assert-equal
   '("0.253759754566d+00" "0.647385390949d+00" "0.219591592741d+01"
     "0.943104910060d+01")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (cylindrical-bessel-Kn-array 2.0d0 besarr 2)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "0.609297669257d+00"
   (cylindrical-bessel-K0-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.681575945186d+00"
   (cylindrical-bessel-K1-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.950085641850d+00"
   (cylindrical-bessel-Kn-scaled 2 4.0d0))
  (lisp-unit:assert-equal
   '("0.253759754566d+00" "0.647385390949d+00" "0.219591592741d+01"
     "0.943104910060d+01")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (cylindrical-bessel-Kn-array 2.0d0 besarr 2)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "-0.189200623827d+00"
   (spherical-bessel-j0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.116110749259d+00"
   (spherical-bessel-j1 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.276283685771d+00"
   (spherical-bessel-j2 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.229243857955d+00"
   (spherical-bessel-jl 3 4.0d0))
  (lisp-unit:assert-equal
   '("-0.189200623827d+00" "0.116110749259d+00" "0.276283685771d+00"
     "0.229243857955d+00")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (spherical-bessel-jl-array 4.0d0 besarr)
      (data besarr))))
  (lisp-unit:assert-equal
   '("-0.189200623827d+00" "0.116110749259d+00" "0.276283685771d+00"
     "0.229243857955d+00")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (spherical-bessel-jl-steed-array 4.0d0 besarr)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "0.163410905216d+00"
   (spherical-bessel-y0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.230053350131d+00"
   (spherical-bessel-y1 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.912910738232d-02"
   (spherical-bessel-y2 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.912910738232d-02"
   (spherical-bessel-yl 2 4.0d0))
  (lisp-unit:assert-equal
   '("0.163410905216d+00" "0.230053350131d+00" "0.912910738232d-02"
     "-0.218641965903d+00")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (spherical-bessel-yl-array 4.0d0 besarr)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "0.124958067172d+00"
   (spherical-bessel-i0-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.938024160356d-01"
   (spherical-bessel-i1-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.546062551448d-01"
   (spherical-bessel-i2-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.255445971046d-01"
   (spherical-bessel-il-scaled 3 4.0d0))
  (lisp-unit:assert-equal
   '("0.124958067172d+00" "0.938024160356d-01" "0.546062551448d-01"
     "0.255445971046d-01")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (spherical-bessel-il-scaled-array 4.0d0 besarr)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "0.392699081699d+00"
   (spherical-bessel-k0-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.490873852123d+00"
   (spherical-bessel-k1-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.760854470791d+00"
   (spherical-bessel-k2-scaled 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.255445971046d-01"
   (spherical-bessel-kl-scaled 3 4.0d0))
  (lisp-unit:assert-equal
   '("0.392699081699d+00" "0.490873852123d+00" "0.760854470791d+00"
     "0.144194194061d+01")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 4)
      (spherical-bessel-kl-scaled-array 4.0d0 besarr)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "0.430171473876d+00"
   (bessel-jnu 3.0d0 4.0d0))
  (lisp-unit:assert-equal
   '("0.671396707142d+00" "0.513016136562d+00" "0.650081828774d-01")
   (lisp-unit:fp-sequence
    (with-data (besarr vector-double 3)
      (setf (data besarr) #(1.0d0 2.0d0 3.0d0))
      (spherical-Jnu-array 0.5d0 besarr)
      (data besarr))))
  (lisp-unit:assert-first-fp-equal
   "-0.182022115953d+00"
   (bessel-Ynu 3.0d0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.333727577842d+01"
   (bessel-Inu 3.0d0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.611243380297d-01"
   (bessel-Inu-scaled 3.0d0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.298849244168d-01"
   (bessel-Knu 3.0d0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "-0.351040112585d+01"
   (bessel-lnKnu 3.0d0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.163166158704d+01"
   (bessel-Knu-scaled 3.0d0 4.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.149309177085d+02"
   (bessel-zero-J0 5))
  (lisp-unit:assert-first-fp-equal
   "0.164706300509d+02"
   (bessel-zero-J1 5))
  (lisp-unit:assert-first-fp-equal
   "0.179598194950d+02"
   (bessel-zero-Jnu 2.0d0 5)))
