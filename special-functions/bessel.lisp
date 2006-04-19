;********************************************************
; file:        bessel.lisp                               
; description: Bessel functions                          
; date:        Fri Mar 17 2006 - 18:42                   
; author:      Liam M. Healy
; modified:    Wed Apr 19 2006 - 17:06
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

(defun-gsl cylindrical-bessel-Jn-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_Jn_array"
  :documentation
  "The values of the regular cylindrical Bessel functions @math{J_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ (- nmax nmin)))))

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

(defun-gsl cylindrical-bessel-Yn-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_Yn_array"
  :documentation
  "The values of the irregular cylindrical Bessel functions @math{Y_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ (- nmax nmin)))))

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
  "The values of the regular modified cylindrical Bessel functions @math{I_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
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
  "The values of the scaled regular modified cylindrical Bessel functions @math{I_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
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
  "The values of the irregular modified cylindrical Bessel functions @math{K_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ (- nmax nmin)))))

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

(defun-gsl cylindrical-bessel-Kn-scaled-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_Kn_scaled_array"
  :documentation
  ;; Bug in original documentation?  doesn't say "modified"
  "The values of the scaled irregular modified cylindrical Bessel functions @math{K_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ (- nmax nmin)))))

;;;;****************************************************************************
;;;; Regular Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-j0 ((x :double))
  "gsl_sf_bessel_j0_e"
  :documentation
  "The regular spherical Bessel function of zeroth order, @math{j_0(x) = \sin(x)/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-j1 ((x :double))
  "gsl_sf_bessel_j1_e"
  :documentation
  "The regular spherical Bessel function of first order, @math{j_1(x) = (\sin(x)/x - \cos(x))/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-j2 ((x :double))
  "gsl_sf_bessel_j2_e"
  :documentation
  "The regular spherical Bessel function of second order, @math{j_2(x) = ((3/x^2 - 1)\sin(x) - 3\cos(x)/x)/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-jl ((l :int) (x :double))
  "gsl_sf_bessel_jl_e"
  :documentation "The regular spherical Bessel function of order @var{l}, @math{j_l(x)}, for @c{$l \geq 0$} @math{l >= 0} and @c{$x \geq 0$} @math{x >= 0}."
  :return (sf-result))

(defun-gsl spherical-bessel-jl-array ((lmax :int) (x :double))
  "gsl_sf_bessel_jl_array"
  :documentation
  "The regular spherical Bessel functions @math{j_l(x)} for @math{l} from 0 to @var{lmax} inclusive  for @c{$lmax \geq 0$} @math{lmax >= 0} and @c{$x \geq 0$} @math{x >= 0}, storing the results in the array @var{result_array}. The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ lmax))))

(defun-gsl spherical-bessel-jl-steed-array ((lmax :int) (x :double))
  "gsl_sf_bessel_jl_steed_array"
  :documentation
  "Uses Steed's method to compute the values of the regular spherical Bessel functions @math{j_l(x)} for @math{l} from 0 to @var{lmax} inclusive for @c{$lmax \geq 0$} @math{lmax >= 0} and @c{$x \geq 0$} @math{x >= 0}, storing the results in the array @var{result_array}. The Steed/Barnett algorithm is described in @cite{Comp. Phys. Comm.} 21, 297 (1981).  Steed's method is more stable than the recurrence used in the other functions but is also slower."
  :return ((:double (1+ lmax))))

;;;;****************************************************************************
;;;; Irregular Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-y0 ((x :double))
  "gsl_sf_bessel_y0_e"
  :documentation
  "The irregular spherical Bessel function of zeroth order, @math{y_0(x) = -\cos(x)/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-y1 ((x :double))
  "gsl_sf_bessel_y1_e"
  :documentation
  "The irregular spherical Bessel function of first order, @math{y_1(x) = -(\cos(x)/x + \sin(x))/x}."
  :return (sf-result))

(defun-gsl spherical-bessel-y2 ((x :double))
  "gsl_sf_bessel_y2_e"
  :documentation
  "The irregular spherical Bessel function of second order, @math{y_2(x) = (-3/x^3 + 1/x)\cos(x) - (3/x^2)\sin(x)}."
  :return (sf-result))

(defun-gsl spherical-bessel-yl ((l :int) (x :double))
  "gsl_sf_bessel_yl_e"
  :documentation
  "The irregular spherical Bessel function of order @var{l}, @math{y_l(x)}, for @c{$l \geq 0$} @math{l >= 0}."
  :return (sf-result))

(defun-gsl spherical-bessel-yl-array ((lmax :int) (x :double))
  "gsl_sf_bessel_yl_array"
  :documentation
  "The irregular spherical Bessel functions @math{y_l(x)} for @math{l} from 0 to @var{lmax} inclusive  for @c{$lmax \geq 0$} @math{lmax >= 0}, storing the results in the array @var{result_array}. The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ lmax))))

;;;;****************************************************************************
;;;; Regular Modified Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-i0-scaled ((x :double))
  "gsl_sf_bessel_i0_scaled_e"
  :documentation
  "The scaled regular modified spherical Bessel function of zeroth order, @math{\exp(-|x|) i_0(x)}."
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

(defun-gsl spherical-bessel-il-scaled-array ((lmax :int) (x :double))
  "gsl_sf_bessel_il_scaled_array"
  :documentation
  "The values of the scaled regular modified cylindrical Bessel functions @math{\exp(-|x|) i_l(x)} for @math{l} from 0 to @var{lmax} inclusive for @c{$lmax \geq 0$} @math{lmax >= 0}, storing the results in the array @var{result_array}.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ lmax))))

;;;;****************************************************************************
;;;; Irregular Modified Spherical Bessel Functions
;;;;****************************************************************************

(defun-gsl spherical-bessel-k0-scaled ((x :double))
  "gsl_sf_bessel_k0_scaled_e"
  :documentation
  "The scaled irregular modified spherical Bessel function of zeroth order, @math{\exp(x) k_0(x)}, for @math{x>0}."
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

(defun-gsl spherical-bessel-kl-scaled-array ((lmax :int) (x :double))
  "gsl_sf_bessel_kl_scaled_array"
  :documentation
  "The values of the scaled irregular modified spherical Bessel functions @math{\exp(x) k_l(x)} for @math{l} from 0 to @var{lmax} inclusive for @c{$lmax \geq 0$} @math{lmax >= 0} and @math{x>0}, storing the results in the array @var{result_array}.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (1+ lmax))))

;;;;****************************************************************************
;;;; Regular Bessel Function - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Jnu ((nu :double) (x :double))
  "gsl_sf_bessel_Jnu_e"
  :documentation
  "The regular cylindrical Bessel function of fractional order @math{\nu}, @math{J_\nu(x)}."
  :return (sf-result))

;;; (double @var{nu}, gsl_mode_t @var{mode}, size_t @var{size}, double @var{v}[])
;;; The mode argument comes before v, need to make that an option on
;;; the :mode argument to defun-gsl.  Need to return v as well.
(defun-gsl bessel-sequence-Jnu ((nu :double) (v (:double * t)))
  "gsl_sf_bessel_sequence_Jnu_e"
  :documentation
  "The regular cylindrical Bessel function of
fractional order @math{\nu}, @math{J_\nu(x)}, evaluated at a series of
@math{x} values.  The array @var{v} of length @var{size} contains the
@math{x} values.  They are assumed to be strictly ordered and positive.
The array is over-written with the values of @math{J_\nu(x_i)}."
  :return-input (v)
  :mode 1)

;;;;****************************************************************************
;;;; Irregular Bessel Function - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Ynu ((nu :double) (x :double))
  "gsl_sf_bessel_Ynu_e"
  :documentation
  "The irregular cylindrical Bessel function of fractional order @math{\nu}, @math{Y_\nu(x)}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Regular Modified Bessel Functions - Fractional Order
;;;;****************************************************************************

(defun-gsl bessel-Inu ((nu :double) (x :double))
  "gsl_sf_bessel_Inu_e"
  :documentation
  "The regular modified Bessel function of fractional order @math{\nu}, @math{I_\nu(x)} for @math{x>0}, @math{\nu>0}."
  :return (sf-result))

(defun-gsl bessel-Inu-scaled ((nu :double) (x :double))
  "gsl_sf_bessel_Inu_scaled_e"
  :documentation
  "The scaled regular modified Bessel function of fractional order @math{\nu}, @math{\exp(-|x|)I_\nu(x)} for @math{x>0}, @math{\nu>0}."
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
  "The location of the @var{s}-th positive zero of the Bessel function @math{J_0(x)}."
  :return (sf-result))

(defun-gsl bessel-zero-J1 ((s :int))
  "gsl_sf_bessel_zero_J1_e"
  :documentation
  "The location of the @var{s}-th positive zero of the Bessel function @math{J_1(x)}."
  :return (sf-result))

(defun-gsl bessel-zero-Jnu ((s :int))
  "gsl_sf_bessel_zero_Jnu_e"
  :documentation
  "These routines compute the location of the @var{s}-th positive zero of
the Bessel function @math{J_\nu(x)}.  The current implementation does not
support negative values of @var{nu}."
  :return (sf-result))
