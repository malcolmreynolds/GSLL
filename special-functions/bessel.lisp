;; Bessel functions
;; Liam Healy, Fri Mar 17 2006 - 18:42
;; Time-stamp: <2008-02-16 19:16:11EST bessel.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Regular Cylindrical Bessel Functions
;;;;****************************************************************************

(defmfun cylindrical-bessel-J0 (x)
    "gsl_sf_bessel_J0_e"
  ((x :double) (ret sf-result))
  :documentation			; FDL
  "The regular cylindrical Bessel function of zeroth order, J_0(x).")

(defmfun cylindrical-bessel-J1 (x)
  "gsl_sf_bessel_J1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The regular cylindrical Bessel function of first order, J_1(x).")

(defmfun cylindrical-bessel-Jn (n x)
  "gsl_sf_bessel_Jn_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The regular cylindrical Bessel function of order n, J_n(x).")

(defmfun cylindrical-bessel-Jn-array (x array &optional (nmin 0))
  "gsl_sf_bessel_Jn_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The values of the regular cylindrical Bessel functions J_n(x)
   for n from nmin to nmin+length(array)-1 inclusive.
   The values are computed using recurrence relations for efficiency,
   and therefore may differ slightly from the exact values.")

;;;;****************************************************************************
;;;; Irregular Cylindrical Bessel Functions
;;;;****************************************************************************

(defmfun cylindrical-bessel-Y0 (x)
  "gsl_sf_bessel_Y0_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular cylindrical Bessel function of zeroth order, Y_0(x).")

(defmfun cylindrical-bessel-Y1 (x)
  "gsl_sf_bessel_Y1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular cylindrical Bessel function of first order, Y_1(x).")

(defmfun cylindrical-bessel-Yn (n x)
  "gsl_sf_bessel_Yn_e"
  ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular cylindrical Bessel function of order n, Y_n(x).")

(defmfun cylindrical-bessel-Yn-array (x array &optional (nmin 0))
  "gsl_sf_bessel_Yn_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The values of the irregular cylindrical Bessel functions
   Y_n(x) for n from from nmin to
   nmin+length(array)-1 inclusive.  The values are computed using
   recurrence relations for efficiency, and therefore may differ slightly
   from the exact values.")

;;;;****************************************************************************
;;;; Regular Modified Cylindrical Bessel Functions
;;;;****************************************************************************

(defmfun cylindrical-bessel-I0 (x)
  "gsl_sf_bessel_I0_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The regular modified cylindrical Bessel function of zeroth order, I_0(x).")

(defmfun cylindrical-bessel-I1 (x)
  "gsl_sf_bessel_I1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The regular modified cylindrical Bessel function of first order, I_1(x).")

(defmfun cylindrical-bessel-In (n x)
  "gsl_sf_bessel_In_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The regular modified cylindrical Bessel function of order n, I_n(x).")

(defmfun cylindrical-bessel-In-array (x array &optional (nmin 0))
  "gsl_sf_bessel_In_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The values of the regular modified cylindrical Bessel functions
   I_n(x) for n from from nmin to nmin+length(array)-1 inclusive.
   The values are computed using recurrence relations for efficiency, and
   therefore may differ slightly from the exact values.")

(defmfun cylindrical-bessel-I0-scaled (x)
  "gsl_sf_bessel_I0_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled regular modified cylindrical Bessel function of zeroth order,
  \exp(-|x|) I_0(x).")

(defmfun cylindrical-bessel-I1-scaled (x)
  "gsl_sf_bessel_I1_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled regular modified cylindrical Bessel function of first order,
  \exp(-|x|) I_1(x).")

(defmfun cylindrical-bessel-In-scaled (n x)
  "gsl_sf_bessel_In_scaled_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled regular modified cylindrical Bessel function of order n,
  \exp(-|x|) I_n(x)}.")

(defmfun cylindrical-bessel-In-scaled-array (x array &optional (nmin 0))
  "gsl_sf_bessel_In_scaled_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  ;; Bug in original documentation?  doesn't say "modified"
  "The values of the scaled regular modified cylindrical Bessel
  functions I_n(x) for n from from nmin to nmin+length(array)-1
  inclusive. The values are computed using recurrence
  relations for efficiency, and therefore may differ slightly from the
  exact values.")

;;;;****************************************************************************
;;;; Irregular Modified Cylindrical Bessel Functions
;;;;****************************************************************************

(defmfun cylindrical-bessel-K0 (x)
  "gsl_sf_bessel_K0_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular modified cylindrical Bessel function of zeroth order,
  K_0(x).")

(defmfun cylindrical-bessel-K1 (x)
  "gsl_sf_bessel_K1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular modified cylindrical Bessel function of first order, K_1(x).")

(defmfun cylindrical-bessel-Kn (n x)
  "gsl_sf_bessel_Kn_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular modified cylindrical Bessel function of order n, K_n(x).")

(defmfun cylindrical-bessel-Kn-array (x array &optional (nmin 0))
  "gsl_sf_bessel_Kn_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The values of the irregular modified cylindrical Bessel functions
   K_n(x) for n from from nmin to nmin+length(array)-1 inclusive.
   The values are computed using recurrence relations for efficiency, and
   therefore may differ slightly from the exact values.")

(defmfun cylindrical-bessel-K0-scaled (x)
  "gsl_sf_bessel_K0_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled irregular modified cylindrical Bessel function of zeroth order,
  \exp(-|x|) K_0(x).")

(defmfun cylindrical-bessel-K1-scaled (x)
  "gsl_sf_bessel_K1_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled irregular modified cylindrical Bessel function of first order,
   \exp(-|x|) K_1(x).")

(defmfun cylindrical-bessel-Kn-scaled (n x)
  "gsl_sf_bessel_Kn_scaled_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled irregular modified cylindrical Bessel function of order n,
  \exp(-|x|) K_n(x).")

(defmfun cylindrical-bessel-Kn-scaled-array (x array &optional (nmin 0))
  "gsl_sf_bessel_Kn_scaled_array"
  ((nmin :int) ((+ nmin (dim0 array) -1) :int) (x :double)
   ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The values of the scaled irregular cylindrical
   Bessel functions \exp(x) K_n(x) for n from from nmin to
   nmin+length(array)-1 inclusive.
   The start of the range nmin must be positive
   or zero.  The domain of the function is x>0. The values are
   computed using recurrence relations for efficiency, and therefore
   may differ slightly from the exact values.")

;;;;****************************************************************************
;;;; Regular Spherical Bessel Functions
;;;;****************************************************************************

(defmfun spherical-bessel-j0 (x)
  "gsl_sf_bessel_j0_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The regular spherical Bessel function of zeroth order, j_0(x) = \sin(x)/x.")

(defmfun spherical-bessel-j1 (x)
  "gsl_sf_bessel_j1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The regular spherical Bessel function of first order, j_1(x)
   = (\sin(x)/x - \cos(x))/x.")

(defmfun spherical-bessel-j2 (x)
  "gsl_sf_bessel_j2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The regular spherical Bessel function of second order, j_2(x)
   = ((3/x^2 - 1)\sin(x) - 3\cos(x)/x)/x.")

(defmfun spherical-bessel-jl (l x)
  "gsl_sf_bessel_jl_e" ((l :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The regular spherical Bessel function of order
   l, j_l(x), for l >= 0 and x >= 0.")

(defmfun spherical-bessel-jl-array (x array)
  "gsl_sf_bessel_jl_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The values of the regular spherical Bessel
  functions j_l(x) for l from 0 to length(array)-1 and x >= 0.
  The values are computed using recurrence relations for
  efficiency, and therefore may differ slightly from the exact values.")

(defmfun spherical-bessel-jl-steed-array (x array)
  "gsl_sf_bessel_jl_steed_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "Uses Steed's method to compute the values of the regular spherical
  Bessel functions j_l(x) for l from 0 to length(array)-1
  inclusive for x >= 0. The Steed/Barnett algorithm is described in
  Comp. Phys. Comm. 21, 297 (1981).  Steed's method is more
  stable than the recurrence used in the other functions but is also
  slower.")

;;;;****************************************************************************
;;;; Irregular Spherical Bessel Functions
;;;;****************************************************************************

(defmfun spherical-bessel-y0 (x)
  "gsl_sf_bessel_y0_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular spherical Bessel function of zeroth order,
  y_0(x) = -\cos(x)/x.")

(defmfun spherical-bessel-y1 (x)
  "gsl_sf_bessel_y1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular spherical Bessel function of first order,
  y_1(x) = -(\cos(x)/x + \sin(x))/x.")

(defmfun spherical-bessel-y2 (x)
  "gsl_sf_bessel_y2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular spherical Bessel function of second order,
  y_2(x) = (-3/x^3 + 1/x)\cos(x) - (3/x^2)\sin(x).")

(defmfun spherical-bessel-yl (l x)
  "gsl_sf_bessel_yl_e" ((l :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular spherical Bessel function of order l, y_l(x), for l >= 0.")

(defmfun spherical-bessel-yl-array (x array)
  "gsl_sf_bessel_yl_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The irregular spherical Bessel functions y_l(x) for l
  from 0 to length(array)-1.  The values are computed using
  recurrence relations for efficiency,
  and therefore may differ slightly from the exact values.")

;;;;****************************************************************************
;;;; Regular Modified Spherical Bessel Functions
;;;;****************************************************************************

(defmfun spherical-bessel-i0-scaled (x)
  "gsl_sf_bessel_i0_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled regular modified spherical Bessel function of zeroth
  order, \exp(-|x|) i_0(x).")

(defmfun spherical-bessel-i1-scaled (x)
  "gsl_sf_bessel_i1_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled regular modified spherical Bessel function of first order,
  \exp(-|x|) i_1(x).")

(defmfun spherical-bessel-i2-scaled (x)
  "gsl_sf_bessel_i2_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled regular modified spherical Bessel function of second order,
   \exp(-|x|) i_2(x).")

(defmfun spherical-bessel-il-scaled (n x)
  "gsl_sf_bessel_il_scaled_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled regular modified spherical Bessel function of order l,
   \exp(-|x|) i_l(x).")

(defmfun spherical-bessel-il-scaled-array (x array)
  "gsl_sf_bessel_il_scaled_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The values of the scaled regular modified cylindrical Bessel
  functions \exp(-|x|) i_l(x) for l from 0 to length(array)-1
  inclusive.  The values are computed using recurrence relations
  for efficiency, and therefore may differ slightly from the exact values.")

;;;;****************************************************************************
;;;; Irregular Modified Spherical Bessel Functions
;;;;****************************************************************************

(defmfun spherical-bessel-k0-scaled (x)
  "gsl_sf_bessel_k0_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled irregular modified spherical Bessel function of zeroth
  order, \exp(x) k_0(x), for x>0.")

(defmfun spherical-bessel-k1-scaled (x)
  "gsl_sf_bessel_k1_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled irregular modified spherical Bessel function of first order,
   \exp(x) k_1(x), for x>0.")

(defmfun spherical-bessel-k2-scaled (x)
  "gsl_sf_bessel_k2_scaled_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled irregular modified spherical Bessel function of second order,
  \exp(x) k_2(x), for x>0.")

(defmfun spherical-bessel-kl-scaled (n x)
  "gsl_sf_bessel_il_scaled_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled irregular modified spherical Bessel function of order l,
   \exp(x) k_l(x), for x>0.")

(defmfun spherical-bessel-kl-scaled-array (x array)
  "gsl_sf_bessel_kl_scaled_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :invalidate (array)
  :documentation			; FDL
  "The values of the scaled irregular modified spherical Bessel
  functions \exp(x) k_l(x) for l from 0 to length(array)-1
  inclusive x>0.  The values are computed using recurrence
  relations for efficiency, and therefore may differ slightly
  from the exact values.")

;;;;****************************************************************************
;;;; Regular Bessel Function - Fractional Order
;;;;****************************************************************************

(defmfun bessel-Jnu (nu x)
  "gsl_sf_bessel_Jnu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The regular cylindrical Bessel function of fractional order
  \nu, J_\nu(x).")

(defmfun spherical-Jnu-array (nu v)
  "gsl_sf_bessel_sequence_Jnu_e"
  ((nu :double) :mode ((dim0 v) :int) ((gsl-array v) :pointer))
  :invalidate (v)
  :documentation			; FDL
  "The regular cylindrical Bessel function of
  fractional order \nu, J_\nu(x), evaluated at a series of
  x values.  The array v contains the x values.
  They are assumed to be strictly ordered and positive.
  The array is over-written with the values of J_\nu(x_i).")

;;;;****************************************************************************
;;;; Irregular Bessel Function - Fractional Order
;;;;****************************************************************************

(defmfun bessel-Ynu (nu x)
  "gsl_sf_bessel_Ynu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular cylindrical Bessel function of fractional order
  \nu, Y_\nu(x).")

;;;;****************************************************************************
;;;; Regular Modified Bessel Functions - Fractional Order
;;;;****************************************************************************

(defmfun bessel-Inu (nu x)
  "gsl_sf_bessel_Inu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The regular modified Bessel function of fractional order
  \nu, I_\nu(x) for x>0, \nu>0.")

(defmfun bessel-Inu-scaled (nu x)
  "gsl_sf_bessel_Inu_scaled_e" ((nu :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled regular modified Bessel function of fractional order
  \nu, \exp(-|x|)I_\nu(x) for x>0, \nu>0.")

;;;;****************************************************************************
;;;; Irregular Modified Bessel Functions - Fractional Order
;;;;****************************************************************************

(defmfun bessel-Knu (nu x)
  "gsl_sf_bessel_Knu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular modified Bessel function of fractional order \nu,
   K_\nu(x) for x>0, \nu>0.")

(defmfun bessel-lnKnu (nu x)
  "gsl_sf_bessel_lnKnu_e" ((nu :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The logarithm of the irregular modified Bessel function of fractional
   order \nu, \ln(K_\nu(x)) for x>0, \nu>0.")

(defmfun bessel-Knu-scaled (nu x)
  "gsl_sf_bessel_Knu_scaled_e" ((nu :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The scaled irregular modified Bessel function of fractional order
   \nu, \exp(+|x|) K_\nu(x) for x>0, \nu>0.")

;;;;****************************************************************************
;;;; Zeros of Regular Bessel Functions
;;;;****************************************************************************

(defmfun bessel-zero-J0 (s)
  "gsl_sf_bessel_zero_J0_e" ((s :int) (ret sf-result))
  :documentation			; FDL
  "The location of the s-th positive zero of the Bessel function
  J_0(x).")

(defmfun bessel-zero-J1 (s)
  "gsl_sf_bessel_zero_J1_e" ((s :int) (ret sf-result))
  :documentation			; FDL
  "The location of the s-th positive zero of the Bessel function
  J_1(x).")

(defmfun bessel-zero-Jnu (nu s)
  "gsl_sf_bessel_zero_Jnu_e" ((nu :double) (s :int) (ret sf-result))
  :documentation			; FDL
  "These routines compute the location of the s-th positive zero
  of the Bessel function J_\nu(x).  The current implementation
  does not support negative values of nu.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|

;; This bizarrely causes a division-by-zero error 
;;	    (bessel-jnu 3.0d0 4.0d0)
;; in macroexpansion the form from within SLIME using SBCL (expands
;; fine from shell)

(make-tests bessel
	    (cylindrical-bessel-J0 4.0d0)
	    (cylindrical-bessel-J1 4.0d0)
	    (cylindrical-bessel-Jn 2 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (cylindrical-bessel-Jn-array 2.0d0 besarr 2)
	      (data besarr))
	    (cylindrical-bessel-Y0 4.0d0)
	    (cylindrical-bessel-Y1 4.0d0)
	    (cylindrical-bessel-Yn 3 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (cylindrical-bessel-Yn-array 2.0d0 besarr 2)
	      (data besarr))
	    (cylindrical-bessel-I0 4.0d0)
	    (cylindrical-bessel-I1 4.0d0)
	    (cylindrical-bessel-In 3 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (cylindrical-bessel-In-array 2.0d0 besarr 2)
	      (data besarr))
	    (cylindrical-bessel-I0-scaled 4.0d0)
	    (cylindrical-bessel-I1-scaled 4.0d0)
	    (cylindrical-bessel-In-scaled 3 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (cylindrical-bessel-In-scaled-array 2.0d0 besarr 2)
	      (data besarr))
	    (cylindrical-bessel-K0 4.0d0)
	    (cylindrical-bessel-K1 4.0d0)
	    (cylindrical-bessel-Kn 2 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (cylindrical-bessel-Kn-array 2.0d0 besarr 2)
	      (data besarr))
	    (cylindrical-bessel-K0-scaled 4.0d0)
	    (cylindrical-bessel-K1-scaled 4.0d0)
	    (cylindrical-bessel-Kn-scaled 2 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (cylindrical-bessel-Kn-array 2.0d0 besarr 2)
	      (data besarr))
	    (spherical-bessel-j0 4.0d0)
	    (spherical-bessel-j1 4.0d0)
	    (spherical-bessel-j2 4.0d0)
	    (spherical-bessel-jl 3 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (spherical-bessel-jl-array 4.0d0 besarr)
	      (data besarr))
	    (letm ((besarr (vector-double 4)))
	      (spherical-bessel-jl-steed-array 4.0d0 besarr)
	      (data besarr))
	    (spherical-bessel-y0 4.0d0)
	    (spherical-bessel-y1 4.0d0)
	    (spherical-bessel-y2 4.0d0)
	    (spherical-bessel-yl 2 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (spherical-bessel-yl-array 4.0d0 besarr)
	      (data besarr))
	    (spherical-bessel-i0-scaled 4.0d0)
	    (spherical-bessel-i1-scaled 4.0d0)
	    (spherical-bessel-i2-scaled 4.0d0)
	    (spherical-bessel-il-scaled 3 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (spherical-bessel-il-scaled-array 4.0d0 besarr)
	      (data besarr))
	    (spherical-bessel-k0-scaled 4.0d0)
	    (spherical-bessel-k1-scaled 4.0d0)

	    (spherical-bessel-k2-scaled 4.0d0)
	    (spherical-bessel-kl-scaled 3 4.0d0)
	    (letm ((besarr (vector-double 4)))
	      (spherical-bessel-kl-scaled-array 4.0d0 besarr)
	      (data besarr))
	    (bessel-jnu 3.0d0 4.0d0)
	    (letm ((besarr (vector-double #(1.0d0 2.0d0 3.0d0))))
	      (spherical-Jnu-array 0.5d0 besarr)
	      (data besarr))
	    (bessel-Ynu 3.0d0 4.0d0)
	    (bessel-Inu 3.0d0 4.0d0)
	    (bessel-Inu-scaled 3.0d0 4.0d0)
	    (bessel-Knu 3.0d0 4.0d0)
	    (bessel-lnKnu 3.0d0 4.0d0)
	    (bessel-Knu-scaled 3.0d0 4.0d0)
	    (bessel-zero-J0 5)
	    (bessel-zero-J1 5)
	    (bessel-zero-Jnu 2.0d0 5))

|#

(LISP-UNIT:DEFINE-TEST BESSEL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.3971498098638474d0 4.334456411751256d-16)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-J0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.0660433280235491d0 2.1409770694795335d-16)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-J1 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3641281458520729d0 3.974061014982464d-16)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-JN 2 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.35283402861563773d0 0.12894324947440206d0
      0.033995719807568436d0 0.007039629755871686d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (CYLINDRICAL-BESSEL-JN-ARRAY 2.0d0 BESARR 2)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.016940739325064968d0 1.8993556609468549d-16)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-Y0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3979257105570999d0 3.1396236150465943d-16)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-Y1 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.18202211595348539d0 3.355735727760045d-16)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-YN 3 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(-0.6174081041906827d0 -1.127783776840428d0
      -2.7659432263306014d0 -9.935989128481978d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (CYLINDRICAL-BESSEL-YN-ARRAY 2.0d0 BESARR 2)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 11.301921952136329d0 2.7297681442535893d-14)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-I0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 9.759465153704449d0 1.9210136786427457d-14)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-I1 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3.3372757784203446d0 8.06056628872663d-15)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-IN 3 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.6889484476987382d0 0.21273995923985267d0
      0.05072856997918024d0 0.009825679323131702d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (CYLINDRICAL-BESSEL-IN-ARRAY 2.0d0 BESARR 2)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.2070019212239867d0 2.241925168997723d-16)
   (MULTIPLE-VALUE-LIST
    (CYLINDRICAL-BESSEL-I0-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1787508395024353d0 1.1370197115937822d-16)
   (MULTIPLE-VALUE-LIST
    (CYLINDRICAL-BESSEL-I1-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0611243380296663d0 9.334510342661594d-17)
   (MULTIPLE-VALUE-LIST
    (CYLINDRICAL-BESSEL-IN-SCALED 3 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.09323903330473338d0 0.028791222639470898d0
      0.006865365386320685d0 0.0013297610941881578d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (CYLINDRICAL-BESSEL-IN-SCALED-ARRAY 2.0d0 BESARR
					  2)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.011159676085853023d0 2.0424662435034432d-17)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-K0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.01248349888726843d0 1.767412161819488d-17)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-K1 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.017401425529487143d0 2.257461693414273d-16)
   (MULTIPLE-VALUE-LIST (CYLINDRICAL-BESSEL-KN 2 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.2537597545660558d0 0.6473853909486341d0
      2.1959159274119586d0 9.431049100596468d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (CYLINDRICAL-BESSEL-KN-ARRAY 2.0d0 BESARR 2)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6092976692566953d0 3.0340122249326356d-16)
   (MULTIPLE-VALUE-LIST
    (CYLINDRICAL-BESSEL-K0-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.681575945185671d0 3.596132979136138d-16)
   (MULTIPLE-VALUE-LIST
    (CYLINDRICAL-BESSEL-K1-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9500856418495256d0 1.1481477659153143d-14)
   (MULTIPLE-VALUE-LIST
    (CYLINDRICAL-BESSEL-KN-SCALED 2 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.2537597545660558d0 0.6473853909486341d0
      2.1959159274119586d0 9.431049100596468d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (CYLINDRICAL-BESSEL-KN-ARRAY 2.0d0 BESARR 2)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.18920062382698205d0 1.6804391107692678d-16)
   (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-J0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.11611074925915747d0 2.387125482192573d-16)
   (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-J1 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.27628368577135015d0 3.680838111259856d-16)
   (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-J2 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.22924385795503022d0 4.581212568400321d-16)
   (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-JL 3 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(-0.18920062382698202d0 0.11611074925915743d0
      0.2762836857713501d0 0.22924385795503022d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (SPHERICAL-BESSEL-JL-ARRAY 4.0d0 BESARR)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(-0.18920062382698208d0 0.11611074925915742d0
      0.27628368577135015d0 0.22924385795503024d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (SPHERICAL-BESSEL-JL-STEED-ARRAY 4.0d0 BESARR)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.16341090521590299d0 1.4513803955642766d-16)
   (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-Y0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.2300533501309578d0 1.5324631572452525d-16)
   (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-Y1 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.009129107382315343d0 1.6876604955506113d-16)
   (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-Y2 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.009129107382315343d0 1.6876604955506113d-16)
   (MULTIPLE-VALUE-LIST (SPHERICAL-BESSEL-YL 2 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.16341090521590299d0 0.2300533501309578d0
      0.009129107382315343d0 -0.21864196590306362d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (SPHERICAL-BESSEL-YL-ARRAY 4.0d0 BESARR)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.12495806717151219d0 5.5492529314587895d-17)
   (MULTIPLE-VALUE-LIST
    (SPHERICAL-BESSEL-I0-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.09380241603560975d0 4.165664081928078d-17)
   (MULTIPLE-VALUE-LIST
    (SPHERICAL-BESSEL-I1-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.05460625514480487d0 2.425004870012731d-17)
   (MULTIPLE-VALUE-LIST
    (SPHERICAL-BESSEL-I2-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.02554459710460367d0 5.842201171222646d-16)
   (MULTIPLE-VALUE-LIST
    (SPHERICAL-BESSEL-IL-SCALED 3 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.12495806717151212d0 0.09380241603560971d0
      0.05460625514480483d0 0.02554459710460367d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (SPHERICAL-BESSEL-IL-SCALED-ARRAY 4.0d0 BESARR)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.39269908169872414d0 1.743934249004316d-16)
   (MULTIPLE-VALUE-LIST
    (SPHERICAL-BESSEL-K0-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4908738521234052d0 2.1799178112553949d-16)
   (MULTIPLE-VALUE-LIST
    (SPHERICAL-BESSEL-K1-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.760854470791278d0 3.378872607445862d-16)
   (MULTIPLE-VALUE-LIST
    (SPHERICAL-BESSEL-K2-SCALED 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.02554459710460367d0 5.842201171222646d-16)
   (MULTIPLE-VALUE-LIST
    (SPHERICAL-BESSEL-KL-SCALED 3 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.39269908169872414d0 0.4908738521234052d0
      0.760854470791278d0 1.4419419406125027d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE 4)))
      (SPHERICAL-BESSEL-KL-SCALED-ARRAY 4.0d0 BESARR)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.43017147387562193d0 7.641380397338472d-16)
   (MULTIPLE-VALUE-LIST (BESSEL-JNU 3.0d0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.6713967071418024d0 0.5130161365618323d0
      0.06500818287738516d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((BESARR (VECTOR-DOUBLE #(1.0d0 2.0d0 3.0d0))))
      (SPHERICAL-JNU-ARRAY 0.5d0 BESARR)
      (DATA BESARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.1820221159534852d0 2.020851441225493d-15)
   (MULTIPLE-VALUE-LIST (BESSEL-YNU 3.0d0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3.3372757784203437d0 1.1856385307923545d-14)
   (MULTIPLE-VALUE-LIST (BESSEL-INU 3.0d0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.061124338029666284d0 1.3572329489101316d-16)
   (MULTIPLE-VALUE-LIST (BESSEL-INU-SCALED 3.0d0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.029884924416755682d0 1.0617257976532701d-16)
   (MULTIPLE-VALUE-LIST (BESSEL-KNU 3.0d0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -3.5104011258456183d0 4.776268519767339d-15)
   (MULTIPLE-VALUE-LIST (BESSEL-LNKNU 3.0d0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.6316615870352025d0 5.072223134504136d-15)
   (MULTIPLE-VALUE-LIST (BESSEL-KNU-SCALED 3.0d0 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 14.930917708487813d0 4.4792753125463437d-14)
   (MULTIPLE-VALUE-LIST (BESSEL-ZERO-J0 5)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 16.470630050877624d0 3.2941260101755246d-13)
   (MULTIPLE-VALUE-LIST (BESSEL-ZERO-J1 5)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 17.95981949498783d0 3.591963898997566d-14)
   (MULTIPLE-VALUE-LIST (BESSEL-ZERO-JNU 2.0d0 5))))


