;********************************************************
; file:        bessel.lisp                               
; description: Bessel functions                          
; date:        Fri Mar 17 2006 - 18:42                   
; author:      Liam M. Healy
; modified:    Fri Mar 17 2006 - 21:32
;********************************************************

(in-package :gsl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '(cylindrical-bessel-J0 cylindrical-bessel-J1 cylindrical-bessel-Jn
     cylindrical-bessel-Jn-array)))

;;;;****************************************************************************
;;;; Regular Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-sf cylindrical-bessel-J0 ((x :double))
  "gsl_sf_bessel_J0_e"
  :documentation
  "The regular cylindrical Bessel function of zeroth order, @math{J_0(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-J1 ((x :double))
  "gsl_sf_bessel_J1_e"
  :documentation
  "The regular cylindrical Bessel function of first order, @math{J_1(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Jn ((n :int) (x :double))
  "gsl_sf_bessel_Jn_e"
  :documentation
  "The regular cylindrical Bessel function of order @var{n}, @math{J_n(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Jn-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_Jn_array"
  :documentation
  "The values of the regular cylindrical Bessel functions @math{J_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (- nmax nmin))))

;;;;****************************************************************************
;;;; Irregular Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-sf cylindrical-bessel-Y0 ((x :double))
  "gsl_sf_bessel_Y0_e"
  :documentation
  "The irregular cylindrical Bessel function of zeroth order, @math{Y_0(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Y1 ((x :double))
  "gsl_sf_bessel_Y1_e"
  :documentation
  "The irregular cylindrical Bessel function of first order, @math{Y_1(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Yn ((n :int) (x :double))
  "gsl_sf_bessel_Yn_e"
  :documentation
  "The irregular cylindrical Bessel function of order @var{n}, @math{Y_n(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Yn-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_Yn_array"
  :documentation
  "The values of the irregular cylindrical Bessel functions @math{Y_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (- nmax nmin))))

;;;;****************************************************************************
;;;; Regular Modified Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-sf cylindrical-bessel-I0 ((x :double))
  "gsl_sf_bessel_I0_e"
  :documentation
  "The regular modified cylindrical Bessel function of zeroth order, @math{I_0(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-I1 ((x :double))
  "gsl_sf_bessel_I1_e"
  :documentation
  "The regular modified cylindrical Bessel function of first order, @math{I_1(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-In ((n :int) (x :double))
  "gsl_sf_bessel_In_e"
  :documentation
  "The regular modified cylindrical Bessel function of order @var{n}, @math{I_n(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-In-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_In_array"
  :documentation
  "The values of the regular modified cylindrical Bessel functions @math{I_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (- nmax nmin))))

(defun-sf cylindrical-bessel-I0-scaled ((x :double))
  "gsl_sf_bessel_I0_scaled_e"
  :documentation
  "The scaled regular modified cylindrical Bessel function of zeroth order, @math{\exp(-|x|) I_0(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-I1-scaled ((x :double))
  "gsl_sf_bessel_I1_scaled_e"
  :documentation
  "The scaled regular modified cylindrical Bessel function of first order, @math{\exp(-|x|) I_1(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-In-scaled ((n :int) (x :double))
  "gsl_sf_bessel_In_scaled_e"
  :documentation
  "The scaled regular modified cylindrical Bessel function of order @var{n}, @math{\exp(-|x|) I_n(x)}}."
  :return (sf-result))

(defun-sf cylindrical-bessel-In-scaled-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_In_scaled_array"
  :documentation
  ;; Bug in original documentation?  doesn't say "modified"
  "The values of the scaled regular modified cylindrical Bessel functions @math{I_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (- nmax nmin))))

;;;;****************************************************************************
;;;; Irregular Modified Cylindrical Bessel Functions
;;;;****************************************************************************

(defun-sf cylindrical-bessel-K0 ((x :double))
  "gsl_sf_bessel_K0_e"
  :documentation
  "The irregular modified cylindrical Bessel function of zeroth order, @math{K_0(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-K1 ((x :double))
  "gsl_sf_bessel_K1_e"
  :documentation
  "The irregular modified cylindrical Bessel function of first order, @math{K_1(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Kn ((n :int) (x :double))
  "gsl_sf_bessel_Kn_e"
  :documentation
  "The irregular modified cylindrical Bessel function of order @var{n}, @math{K_n(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Kn-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_Kn_array"
  :documentation
  "The values of the irregular modified cylindrical Bessel functions @math{K_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (- nmax nmin))))

(defun-sf cylindrical-bessel-K0-scaled ((x :double))
  "gsl_sf_bessel_K0_scaled_e"
  :documentation
  "The scaled irregular modified cylindrical Bessel function of zeroth order, @math{\exp(-|x|) K_0(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-K1-scaled ((x :double))
  "gsl_sf_bessel_K1_scaled_e"
  :documentation
  "The scaled irregular modified cylindrical Bessel function of first order, @math{\exp(-|x|) K_1(x)}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Kn-scaled ((n :int) (x :double))
  "gsl_sf_bessel_Kn_scaled_e"
  :documentation
  "The scaled irregular modified cylindrical Bessel function of order @var{n}, @math{\exp(-|x|) K_n(x)}}."
  :return (sf-result))

(defun-sf cylindrical-bessel-Kn-scaled-array ((nmin :int) (nmax :int) (x :double))
  "gsl_sf_bessel_Kn_scaled_array"
  :documentation
  ;; Bug in original documentation?  doesn't say "modified"
  "The values of the scaled irregular modified cylindrical Bessel functions @math{K_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  :return ((:double (- nmax nmin))))

;;;;****************************************************************************
;;;; Regular Spherical Bessel Functions
;;;;****************************************************************************

(defun-sf regular-spherical-bessel ((order :int) (x :double))
  "gsl_sf_bessel_jl_e"
  :documentation "The regular spherical Bessel function j_l."
  :return (sf-result))

;;; (regular-spherical-bessel 3 12.4d0)
;;; 0.07813007015855176d0
;;; 1.9083196617232666d-16
