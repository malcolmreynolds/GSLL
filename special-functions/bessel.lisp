;********************************************************
; file:        bessel.lisp                               
; description: Bessel functions                          
; date:        Fri Mar 17 2006 - 18:42                   
; author:      Liam M. Healy
; modified:    Tue May  2 2006 - 12:10
;********************************************************

(in-package :gsl)

;;;;****************************************************************************
;;;; Regular Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-gsl cylindrical-bessel-J0 ((x :double))
  "gsl_sf_bessel_J0_e"
  :documentation
  "The regular cylindrical Bessel function of zeroth order, @math{J_0(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-J1 ((x :double))
  "gsl_sf_bessel_J1_e"
  :documentation
  "The regular cylindrical Bessel function of first order, @math{J_1(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Jn ((n :int) (x :double))
  "gsl_sf_bessel_Jn_e"
  :documentation
  "The regular cylindrical Bessel function of order @var{n}, @math{J_n(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Jn-array
    ((nmin :int) ((+ nmin (dim0 array)) :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_bessel_Jn_array"
  :function (x array &optional (nmin 0))
  :documentation
  "The values of the regular cylindrical Bessel functions @math{J_n(x)}
   for @math{n} from @var{nmin} to @var{nmax} inclusive.
   The values are computed using recurrence relations for efficiency,
   and therefore may differ slightly from the exact values."
  :invalidate (array)
  :return-input (array))

;;; (defparameter besarr (make-data 'vector nil 4))
;;; (cylindrical-bessel-Jn-array 2.0d0 besarr 2)
;;; #<GSL-VECTOR #(0.35283402861563773d0 0.12894324947440206d0
;;;               0.033995719807568436d0 0.007039629755871685d0) {C7D6B51}>

;;;;****************************************************************************
;;;; Irregular Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-gsl cylindrical-bessel-Y0 ((x :double))
  "gsl_sf_bessel_Y0_e"
  :documentation
  "The irregular cylindrical Bessel function of zeroth order, @math{Y_0(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Y1 ((x :double))
  "gsl_sf_bessel_Y1_e"
  :documentation
  "The irregular cylindrical Bessel function of first order, @math{Y_1(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Yn ((n :int) (x :double))
  "gsl_sf_bessel_Yn_e"
  :documentation
  "The irregular cylindrical Bessel function of order @var{n}, @math{Y_n(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Yn-array
    ((nmin :int) ((+ nmin (dim0 array)) :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_bessel_Yn_array"
  :function (x array &optional (nmin 0))
  :documentation
  "The values of the irregular cylindrical Bessel functions
  @math{Y_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.
  The values are computed using recurrence relations for efficiency, and
  therefore may differ slightly from the exact values."
  :invalidate (array)
  :return-input (array))

;;;;****************************************************************************
;;;; Regular Modified Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-gsl cylindrical-bessel-I0 ((x :double))
  "gsl_sf_bessel_I0_e"
  :documentation
  "The regular modified cylindrical Bessel function of zeroth order, @math{I_0(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-I1 ((x :double))
  "gsl_sf_bessel_I1_e"
  :documentation
  "The regular modified cylindrical Bessel function of first order, @math{I_1(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-In ((n :int) (x :double))
  "gsl_sf_bessel_In_e"
  :documentation
  "The regular modified cylindrical Bessel function of order @var{n}, @math{I_n(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-In-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_In_array"
  :documentation
   "The values of the regular modified cylindrical Bessel functions
   @math{I_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.
   The values are computed using recurrence relations for efficiency, and
   therefore may differ slightly from the exact values."
  :return ((:double (1+ (- nmax nmin)))))

(defun-gsl cylindrical-bessel-I0-scaled ((x :double))
  "gsl_sf_bessel_I0_scaled_e"
  :documentation
  "The scaled regular modified cylindrical Bessel function of zeroth order, @math{\exp(-|x|) I_0(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-I1-scaled ((x :double))
  "gsl_sf_bessel_I1_scaled_e"
  :documentation
  "The scaled regular modified cylindrical Bessel function of first order, @math{\exp(-|x|) I_1(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-In-scaled ((n :int) (x :double))
  "gsl_sf_bessel_In_scaled_e"
  :documentation
  "The scaled regular modified cylindrical Bessel function of order @var{n}, @math{\exp(-|x|) I_n(x)}}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-In-scaled-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_In_scaled_array"
  :documentation
  ;; Bug in original documentation?  doesn't say "modified"
  "The values of the scaled regular modified cylindrical Bessel
  functions @math{I_n(x)} for @math{n} from @var{nmin} to @var{nmax}
  inclusive.  The values are computed using recurrence relations for
  efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ (- nmax nmin)))))

;;;;****************************************************************************
;;;; Irregular Modified Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-gsl cylindrical-bessel-K0 ((x :double))
  "gsl_sf_bessel_K0_e"
  :documentation
  "The irregular modified cylindrical Bessel function of zeroth order, @math{K_0(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-K1 ((x :double))
  "gsl_sf_bessel_K1_e"
  :documentation
  "The irregular modified cylindrical Bessel function of first order, @math{K_1(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Kn ((n :int) (x :double))
  "gsl_sf_bessel_Kn_e"
  :documentation
  "The irregular modified cylindrical Bessel function of order @var{n}, @math{K_n(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Kn-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_Kn_array"
  :documentation
   "The values of the irregular modified cylindrical Bessel functions
   @math{K_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.
   The values are computed using recurrence relations for efficiency, and
   therefore may differ slightly from the exact values."  :return
((:double (1+ (- nmax nmin)))))

(defun-gsl cylindrical-bessel-K0-scaled ((x :double))
  "gsl_sf_bessel_K0_scaled_e"
  :documentation
  "The scaled irregular modified cylindrical Bessel function of zeroth order, @math{\exp(-|x|) K_0(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-K1-scaled ((x :double))
  "gsl_sf_bessel_K1_scaled_e"
  :documentation
  "The scaled irregular modified cylindrical Bessel function of first order, @math{\exp(-|x|) K_1(x)}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Kn-scaled ((n :int) (x :double))
  "gsl_sf_bessel_Kn_scaled_e"
  :documentation
  "The scaled irregular modified cylindrical Bessel function of order @var{n}, @math{\exp(-|x|) K_n(x)}}."
  :return (sf-result))

(defun-gsl cylindrical-bessel-Kn-scaled-array
    ((nmin :int) ((+ nmin (dim0 array)) :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_bessel_Kn_scaled_array"
  :function (x array &optional (nmin 0))
  :documentation
  ;; Bug in original documentation?  doesn't say "modified"
  "The values of the scaled irregular modified cylindrical Bessel
  functions @math{K_n(x)} for @math{n} from @var{nmin} to @var{nmax}
  inclusive.  The values are computed using recurrence relations for
  efficiency, and therefore may differ slightly from the exact values."
  :invalidate (array)
  :return-input (array))

;;;;****************************************************************************
;;;; Regular Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-j0 ((x :double))
  "gsl_sf_bessel_j0_e"
  :documentation
  "The regular spherical Bessel function of zeroth order, @math{j_0(x)
  = \sin(x)/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-j1 ((x :double))
  "gsl_sf_bessel_j1_e"
  :documentation
  "The regular spherical Bessel function of first order, @math{j_1(x)
   = (\sin(x)/x - \cos(x))/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-j2 ((x :double))
  "gsl_sf_bessel_j2_e"
  :documentation
  "The regular spherical Bessel function of second order, @math{j_2(x)
   = ((3/x^2 - 1)\sin(x) - 3\cos(x)/x)/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-jl ((l :int) (x :double))
  "gsl_sf_bessel_jl_e"
  :documentation "The regular spherical Bessel function of order
  @var{l}, @math{j_l(x)}, for @math{l >= 0} and @math{x >= 0}."
  :return (sf-result))

(defun-gsl spherical-bessel-jl-array
    (((dim0 array) :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_bessel_jl_array"
  :function (x array)
  :documentation
  "The regular spherical Bessel functions @math{j_l(x)} for @math{l}
  from 0 to @var{lmax} inclusive for @math{lmax >= 0}
  and @math{x >= 0}, storing the results in the array
  @var{result_array}. The values are computed using recurrence relations
  for efficiency, and therefore may differ slightly from the exact
  values."
  :invalidate (array)
  :return-input (array))

(defun-gsl spherical-bessel-jl-steed-array
    (((dim0 array) :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_bessel_jl_steed_array"
  :function (x array)
  :documentation
  "Uses Steed's method to compute the values of the regular spherical
  Bessel functions @math{j_l(x)} for @math{l} from 0 to @var{lmax}
  inclusive for @math{lmax >= 0} and @math{x >= 0},
  storing the results in the array
  @var{result_array}. The Steed/Barnett algorithm is described in
  @cite{Comp. Phys. Comm.} 21, 297 (1981).  Steed's method is more
  stable than the recurrence used in the other functions but is also
  slower."
  :invalidate (array)
  :return-input (array))

;;;;****************************************************************************
;;;; Irregular Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-y0 ((x :double))
  "gsl_sf_bessel_y0_e"
  :documentation
  "The irregular spherical Bessel function of zeroth order,
  @math{y_0(x) = -\cos(x)/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-y1 ((x :double))
  "gsl_sf_bessel_y1_e"
  :documentation
  "The irregular spherical Bessel function of first order,
  @math{y_1(x) = -(\cos(x)/x + \sin(x))/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-y2 ((x :double))
  "gsl_sf_bessel_y2_e"
  :documentation
  "The irregular spherical Bessel function of second order,
  @math{y_2(x) = (-3/x^3 + 1/x)\cos(x) - (3/x^2)\sin(x)}."
  :return (sf-result))

(defun-gsl spherical-bessel-yl ((l :int) (x :double))
  "gsl_sf_bessel_yl_e"
  :documentation
  "The irregular spherical Bessel function of order @var{l},
  @math{y_l(x)}, for @math{l >= 0}."
  :return (sf-result))

(defun-gsl spherical-bessel-yl-array
    (((dim0 array) :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_bessel_yl_array"
  :function (x array)
  :documentation
  "The irregular spherical Bessel functions @math{y_l(x)} for @math{l}
  from 0 to @var{lmax} inclusive for @math{lmax >= 0},
  storing the results in the array @var{result_array}. The values are
  computed using recurrence relations for efficiency, and therefore may
  differ slightly from the exact values."
  :invalidate (array)
  :return-input (array))

;;;;****************************************************************************
;;;; Regular Modified Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-i0-scaled ((x :double))
  "gsl_sf_bessel_i0_scaled_e"
  :documentation
  "The scaled regular modified spherical Bessel function of zeroth
  order, @math{\exp(-|x|) i_0(x)}."
  :return (sf-result))

(defun-gsl spherical-bessel-i1-scaled ((x :double))
  "gsl_sf_bessel_i1_scaled_e"
  :documentation
  "The scaled regular modified spherical Bessel function of first order, @math{\exp(-|x|) i_1(x)}."
  :return (sf-result))

(defun-gsl spherical-bessel-i2-scaled ((x :double))
  "gsl_sf_bessel_i2_scaled_e"
  :documentation
  "The scaled regular modified spherical Bessel function of second order, @math{ \exp(-|x|) i_2(x) }."
  :return (sf-result))

(defun-gsl spherical-bessel-il-scaled ((n :int) (x :double))
  "gsl_sf_bessel_il_scaled_e"
  :documentation
  "The scaled regular modified spherical Bessel function of order @var{l}, @math{ \exp(-|x|) i_l(x) }."
  :return (sf-result))

(defun-gsl spherical-bessel-il-scaled-array
    (((dim0 array) :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_bessel_il_scaled_array"
  :function (x array)
  :documentation
  "The values of the scaled regular modified cylindrical Bessel
  functions @math{\exp(-|x|) i_l(x)} for @math{l} from 0 to @var{lmax}
  inclusive for @math{lmax >= 0}, storing the results
  in the array @var{result_array}.  The values are computed using
  recurrence relations for efficiency, and therefore may differ slightly
  from the exact values."
  :invalidate (array)
  :return-input (array))

;;;;****************************************************************************
;;;; Irregular Modified Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-k0-scaled ((x :double))
  "gsl_sf_bessel_k0_scaled_e"
  :documentation
  "The scaled irregular modified spherical Bessel function of zeroth
  order, @math{\exp(x) k_0(x)}, for @math{x>0}."
  :return (sf-result))

(defun-gsl spherical-bessel-k1-scaled ((x :double))
  "gsl_sf_bessel_k1_scaled_e"
  :documentation
  "The scaled irregular modified spherical Bessel function of first order, @math{\exp(x) k_1(x)}, for @math{x>0}."
  :return (sf-result))

(defun-gsl spherical-bessel-k2-scaled ((x :double))
  "gsl_sf_bessel_k2_scaled_e"
  :documentation
  "The scaled irregular modified spherical Bessel function of second order, @math{\exp(x) k_2(x)}, for @math{x>0}."
  :return (sf-result))

(defun-gsl spherical-bessel-kl-scaled ((n :int) (x :double))
  "gsl_sf_bessel_il_scaled_e"
  :documentation
  "The scaled irregular modified spherical Bessel function of order @var{l}, @math{\exp(x) k_l(x)}, for @math{x>0}."
  :return (sf-result))

(defun-gsl spherical-bessel-kl-scaled-array
    (((dim0 array) :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_bessel_kl_scaled_array"
  :function (x array)
  :documentation
  "The values of the scaled irregular modified spherical Bessel
  functions @math{\exp(x) k_l(x)} for @math{l} from 0 to @var{lmax}
  inclusive for @math{lmax >= 0} and @math{x>0},
  storing the results in the array @var{result_array}.  The values are
  computed using recurrence relations for efficiency, and therefore may
  differ slightly from the exact values."
  :invalidate (array)
  :return-input (array))

;;;;****************************************************************************
;;;; Regular Bessel Function - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Jnu ((nu :double) (x :double))
  "gsl_sf_bessel_Jnu_e"
  :documentation
  "The regular cylindrical Bessel function of fractional order
  @math{\nu}, @math{J_\nu(x)}."
  :return (sf-result))

(defun-gsl bessel-sequence-Jnu
    ((nu :double) ((dim0 v) :size) ((gsl-array v) :pointer))
  "gsl_sf_bessel_sequence_Jnu_e"
  :function (nu v)
  :documentation
  "The regular cylindrical Bessel function of
  fractional order @math{\nu}, @math{J_\nu(x)}, evaluated at a series of
  @math{x} values.  The array @var{v} of length @var{size} contains the
  @math{x} values.  They are assumed to be strictly ordered and positive.
  The array is over-written with the values of @math{J_\nu(x_i)}."
  :after ((cl-invalidate v))
  :return-input (v)
  :mode 1)

;;; (defparameter vec (make-data 'vector nil 3))
;;; (setf (data vec) #(1.0d0 2.0d0 3.0d0))
;;; (bessel-sequence-Jnu 0.5d0 vec)

;;;;****************************************************************************
;;;; Irregular Bessel Function - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Ynu ((nu :double) (x :double))
  "gsl_sf_bessel_Ynu_e"
  :documentation
  "The irregular cylindrical Bessel function of fractional order
  @math{\nu}, @math{Y_\nu(x)}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Regular Modified Bessel Functions - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Inu ((nu :double) (x :double))
  "gsl_sf_bessel_Inu_e"
  :documentation
  "The regular modified Bessel function of fractional order
  @math{\nu}, @math{I_\nu(x)} for @math{x>0}, @math{\nu>0}."
  :return (sf-result))

(defun-gsl bessel-Inu-scaled ((nu :double) (x :double))
  "gsl_sf_bessel_Inu_scaled_e"
  :documentation
  "The scaled regular modified Bessel function of fractional order
  @math{\nu}, @math{\exp(-|x|)I_\nu(x)} for @math{x>0}, @math{\nu>0}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Irregular Modified Bessel Functions - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Knu ((nu :double) (x :double))
  "gsl_sf_bessel_Knu_e"
  :documentation
  "The irregular modified Bessel function of fractional order @math{\nu}, @math{K_\nu(x)} for @math{x>0}, @math{\nu>0}."
  :return (sf-result))

(defun-gsl bessel-lnKnu ((nu :double) (x :double))
  "gsl_sf_bessel_lnKnu_e"
  :documentation
  "The logarithm of the irregular modified Bessel function of fractional order @math{\nu}, @math{\ln(K_\nu(x))} for @math{x>0}, @math{\nu>0}."
  :return (sf-result))

(defun-gsl bessel-Knu-scaled ((nu :double) (x :double))
  "gsl_sf_bessel_Knu_scaled_e"
  :documentation
  "The scaled irregular modified Bessel function of fractional order @math{\nu}, @math{\exp(+|x|) K_\nu(x)} for @math{x>0}, @math{\nu>0}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Zeros of Regular Bessel Functions
;;;;****************************************************************************

(defun-gsl bessel-zero-J0 ((s :int))
  "gsl_sf_bessel_zero_J0_e"
  :documentation
  "The location of the @var{s}-th positive zero of the Bessel function
  @math{J_0(x)}."
  :return (sf-result))

(defun-gsl bessel-zero-J1 ((s :int))
  "gsl_sf_bessel_zero_J1_e"
  :documentation
  "The location of the @var{s}-th positive zero of the Bessel function
  @math{J_1(x)}."
  :return (sf-result))

(defun-gsl bessel-zero-Jnu ((nu :double) (s :int))
  "gsl_sf_bessel_zero_Jnu_e"
  :documentation
  "These routines compute the location of the @var{s}-th positive zero
  of the Bessel function @math{J_\nu(x)}.  The current implementation
  does not support negative values of @var{nu}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test bessel
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.397149809864d+00"
   (CYLINDRICAL-BESSEL-J0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.660433280235d-01"
   (CYLINDRICAL-BESSEL-J1 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.364128145852d+00"
   (CYLINDRICAL-BESSEL-JN 2 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.169407393251d-01"
   (CYLINDRICAL-BESSEL-Y0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.397925710557d+00"
   (CYLINDRICAL-BESSEL-Y1 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.182022115953d+00"
   (CYLINDRICAL-BESSEL-YN 3 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.113019219521d+02"
   (CYLINDRICAL-BESSEL-I0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.975946515370d+01"
   (CYLINDRICAL-BESSEL-I1 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.333727577842d+01"
   (CYLINDRICAL-BESSEL-IN 3 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.207001921224d+00"
   (CYLINDRICAL-BESSEL-I0-SCALED 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.178750839502d+00"
   (CYLINDRICAL-BESSEL-I1-SCALED 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.611243380297d-01"
   (CYLINDRICAL-BESSEL-IN-SCALED 3 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.189200623827d+00"
   (SPHERICAL-BESSEL-J0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.116110749259d+00"
   (SPHERICAL-BESSEL-J1 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.276283685771d+00"
   (SPHERICAL-BESSEL-J2 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.229243857955d+00"
   (SPHERICAL-BESSEL-JL 3 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.163410905216d+00"
   (SPHERICAL-BESSEL-Y0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.230053350131d+00"
   (SPHERICAL-BESSEL-Y1 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.912910738232d-02"
   (SPHERICAL-BESSEL-Y2 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.912910738232d-02"
   (SPHERICAL-BESSEL-YL 2 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.124958067172d+00"
   (SPHERICAL-BESSEL-I0-SCALED 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.938024160356d-01"
   (SPHERICAL-BESSEL-I1-SCALED 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.546062551448d-01"
   (SPHERICAL-BESSEL-I2-SCALED 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.255445971046d-01"
   (SPHERICAL-BESSEL-IL-SCALED 3 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.392699081699d+00"
   (SPHERICAL-BESSEL-K0-SCALED 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.490873852123d+00"
   (SPHERICAL-BESSEL-K1-SCALED 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.760854470791d+00"
   (SPHERICAL-BESSEL-K2-SCALED 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.255445971046d-01"
   (SPHERICAL-BESSEL-KL-SCALED 3 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.430171473876d+00"
   (BESSEL-JNU 3.0d0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.182022115953d+00"
   (BESSEL-YNU 3.0d0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.333727577842d+01"
   (BESSEL-INU 3.0d0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.611243380297d-01"
   (BESSEL-INU-SCALED 3.0d0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.298849244168d-01"
   (BESSEL-KNU 3.0d0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.351040112585d+01"
   (BESSEL-LNKNU 3.0d0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.163166158704d+01"
   (BESSEL-KNU-SCALED 3.0d0 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.149309177085d+02"
   (BESSEL-ZERO-J0 5))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.164706300509d+02"
   (BESSEL-ZERO-J1 5))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.179598194950d+02"
   (BESSEL-ZERO-JNU 2.0d0 5)))
