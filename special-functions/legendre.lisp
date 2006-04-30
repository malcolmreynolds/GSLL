;********************************************************
; file:        legendre.lisp                             
; description: Legendre functions                        
; date:        Sat Apr 29 2006 - 19:16                   
; author:      Liam M. Healy                             
; modified:    Sun Apr 30 2006 - 12:25
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Legendre polynomials
;;;;****************************************************************************

(defun-gsl legendre-P1 ((x :double))
  "gsl_sf_legendre_P1_e"
  :documentation
  "The Legendre polynomials @math{P_1(x)} using an explicit
   representation."
  :return (sf-result))

(defun-gsl legendre-P2 ((x :double))
  "gsl_sf_legendre_P2_e"
  :documentation
  "The Legendre polynomials @math{P_2(x)} using an explicit
   representation."
  :return (sf-result))

(defun-gsl legendre-P3 ((x :double))
  "gsl_sf_legendre_P3_e"
  :documentation
  "The Legendre polynomials @math{P_3(x)} using an explicit
   representation."
  :return (sf-result))

(defun-gsl legendre-Pl ((l :int) (x :double))
  "gsl_sf_legendre_Pl_e"
  :documentation
  "The Legendre polynomial @math{P_l(x)} for a specific value of @var{l},
   @var{x} subject to @math{l >= 0}, @math{|x| <= 1}."
  :return (sf-result))

(defun-gsl legendre-Pl-array
    (((dim0 array) :int) (x :double) ((gsl-array array) :pointer))
  "gsl_sf_legendre_Pl_array"
  :documentation "Compute an array of Legendre polynomials
  @math{P_l(x)} for @math{l = 0, \dots, lmax}, @math{|x| <= 1}."
  :function (x array)
  :invalidate (array)
  :return-input (array))

;;; (defparameter leg (make-data 'vector nil 8))
;;; (legendre-Pl-array 0.5d0 leg)
;;; #<GSL-VECTOR #(1.0d0 0.5d0 -0.125d0 -0.4375d0 -0.2890625d0 0.08984375d0
;;;                0.3232421875d0 0.22314453125d0) {BCC2319}>

(defun-gsl legendre-Pl-deriv-array
    (((dim0 array) :int) (x :double) ((gsl-array array) :pointer))
  "gsl_sf_legendre_Pl_deriv_array"
  :documentation "Compute an array of Legendre polynomials derivatives
  @math{dP_l(x)/dx}, for @math{l = 0, \dots, lmax}, @math{|x| <= 1}."
  :function (x array)
  :invalidate (array)
  :return-input (array))

(defun-gsl legendre-Q0 ((x :double))
  "gsl_sf_legendre_Q0_e"
  :documentation
  "The Legendre function @math{Q_0(x)} for @math{x > -1},
   @math{x /= 1}."
  :return (sf-result))

(defun-gsl legendre-Q1 ((x :double))
  "gsl_sf_legendre_Q1_e"
  :documentation
  "The Legendre function @math{Q_1(x)} for @math{x > -1},
   @math{x /= 1}."
  :return (sf-result))

(defun-gsl legendre-Ql ((l :int) (x :double))
  "gsl_sf_legendre_Ql_e"
  :documentation
  "The Legendre function @math{Q_l(x)} for @math{x > -1},
   @math{x /= 1}, @math{l >= 0}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Associated Legendre Polynomials and Spherical Harmonics
;;;;****************************************************************************

;;; The following functions compute the associated Legendre Polynomials
;;; @math{P_l^m(x)}.  Note that this function grows combinatorially with
;;; @math{l} and can overflow for @math{l} larger than about 150.  There is
;;; no trouble for small @math{m}, but overflow occurs when @math{m} and
;;; @math{l} are both large.  Rather than allow overflows, these functions
;;; refuse to calculate @math{P_l^m(x)} and return @code{GSL_EOVRFLW} when
;;; they can sense that @math{l} and @math{m} are too big.

;;; If you want to calculate a spherical harmonic, then @emph{do not} use
;;; these functions.  Instead use @code{gsl_sf_legendre_sphPlm()} below,
;;; which uses a similar recursion, but with the normalized functions.

(defun-gsl legendre-Plm ((l :int) (m :int) (x :double))
  "gsl_sf_legendre_Plm_e"
  :documentation
  "The associated Legendre polynomial
   @math{P_l^m(x)} for @math{m >= 0}, @math{l >= m}, @math{|x| <= 1}."
  :return (sf-result))


(defun-gsl legendre-Plm-array
    (((+ (dim0 array) m -1) :int) (m :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_legendre_Plm_array"
  :documentation "An array of Legendre polynomials
    @math{P_l^m(x)}, for @math{m >= 0}, 
    @math{l = |m|, ..., lmax}, where lmax is the length
    of the vector array, and @math{|x| <= 1}."
  :function (m x array)
  :invalidate (array)
  :return-input (array))

;;; (defparameter aleg (make-data 'vector nil 3))
;;; (legendre-plm-array 3 0.5d0 aleg)
;;; #<GSL-VECTOR #(-9.742785792574935d0 -34.09975027401227d0 -42.62468784251534d0) {BD445E9}>

(defun-gsl legendre-Plm-deriv-array
    (((+ (dim0 array) m -1) :int) (m :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_legendre_Plm_deriv_array"
  :documentation "An array of Legendre polynomials derivatives
    @math{dP_l^m(x)/dx} for @math{m >= 0}, 
    @math{l = |m|, ..., lmax}, where lmax is the length
    of the vector array, and @math{|x| <= 1}."
  :function (m x array)
  :invalidate (array)
  :return-input (array))

(defun-gsl legendre-sphPlm ((l :int) (m :int) (x :double))
  "gsl_sf_legendre_sphPlm_e"
  :documentation
  "The normalized associated Legendre polynomial
   @math{$\sqrt@{(2l+1)/(4\pi)@} \sqrt@{(l-m)!/(l+m)!@} P_l^m(x)$} suitable
   for use in spherical harmonics.  The parameters must satisfy
   @math{m >= 0}, @math{l >= m}, @math{|x| <= 1}.
   These routines avoid the overflows that occur for the standard
   normalization of @math{P_l^m(x)}."
  :return (sf-result))

(defun-gsl legendre-sphPlm-array
    (((+ (dim0 array) m -1) :int) (m :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_legendre_sphPlm_array"
  :documentation "An array of normalized associated Legendre functions
   @math{$\sqrt@{(2l+1)/(4\pi)@} \sqrt@{(l-m)!/(l+m)!@} P_l^m(x)$},
   for @math{m >= 0}, @math{l = |m|, ..., lmax}, @math{|x| <= 1.0}."
  :function (m x array)
  :invalidate (array)
  :return-input (array))

(defun-gsl legendre-sphPlm-deriv-array
    (((+ (dim0 array) m -1) :int) (m :int) (x :double)
     ((gsl-array array) :pointer))
  "gsl_sf_legendre_sphPlm_deriv_array"
  :documentation "An array of normalized associated Legendre functions
   derivatives for @math{m >= 0}, @math{l = |m|, ..., lmax}, @math{|x| <= 1.0}."
  :function (m x array)
  :invalidate (array)
  :return-input (array))

(defun-gsl legendre-array-size ((lmax :int) (m :int))
  "gsl_sf_legendre_array_size"
  :documentation "The size of @var{result_array}[] needed for the array
    versions of @math{P_l^m(x)}, @math{@var{lmax} - @var{m} + 1}."
  :c-return-value :return
  :return (:int))

;;;;****************************************************************************
;;;; Conical Functions
;;;;****************************************************************************

;;; The Conical Functions @math{P^\mu_@{-(1/2)+i\lambda@}(x)} and 
;;; @math{Q^\mu_@{-(1/2)+i\lambda@}} 
;;; are described in Abramowitz & Stegun, Section 8.12.

(defun-gsl legendre-conicalP-half ((lambda :double) (x :double))
  "gsl_sf_conicalP_half_e"
  :documentation "The irregular Spherical Conical Function
   @math{P^@{1/2@}_@{-1/2 + i \lambda@}(x)} for @math{x > -1}."
  :return (sf-result))

(defun-gsl legendre-conicalP-mhalf ((lambda :double) (x :double))
  "gsl_sf_conicalP_mhalf_e"
  :documentation "The regular Spherical Conical Function
  @math{P^@{-1/2@}_@{-1/2 + i \lambda@}(x)} for @math{x > -1}."
  :return (sf-result))

(defun-gsl legendre-conicalP-0 ((lambda :double) (x :double))
  "gsl_sf_conicalP_0_e"
  :documentation "The conical function @math{P^0_@{-1/2 + i \lambda@}(x)}
  for @math{x > -1}."
  :return (sf-result))

(defun-gsl legendre-conicalP-1 ((lambda :double) (x :double))
  "gsl_sf_conicalP_1_e"
  :documentation "The conical function 
   @math{P^1_@{-1/2 + i \lambda@}(x)} for @math{x > -1}."
  :return (sf-result))

(defun-gsl legendre-regular-spherical-conical
    ((l :int) (lambda :double) (x :double))
  "gsl_sf_conicalP_sph_reg_e"
  :documentation "The Regular Spherical Conical Function
   @math{P^@{-1/2-l@}_@{-1/2 + i \lambda@}(x)} for @math{x > -1},
   @math{l >= -1}."
  :return (sf-result))

(defun-gsl legendre-regular-cylindrical-conical
    ((l :int) (lambda :double) (x :double))
  "gsl_sf_conicalP_cyl_reg_e"
  :documentation "The Regular Cylindrical Conical Function
   @math{P^@{-m@}_@{-1/2 + i \lambda@}(x)} for @math{x > -1},
   @math{m >= -1}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Radial Functions for Hyperbolic Space
;;;;****************************************************************************

;;; The following spherical functions are specializations of Legendre
;;; functions which give the regular eigenfunctions of the Laplacian on a
;;; 3-dimensional hyperbolic space @math{H3d}.  Of particular interest is
;;; the flat limit, @math{\lambda \to \infty}, @math{\eta \to 0},
;;; @math{\lambda\eta} fixed.

(defun-gsl legendre-H3d-0 ((lambda :double) (eta :double))
  "gsl_sf_legendre_H3d_0_e"
  :documentation "The zeroth radial eigenfunction of the Laplacian on the
   3-dimensional hyperbolic space,
   @math{L^@{H3d@}_0(\lambda,\eta) := \sin(\lambda\eta)/(\lambda\sinh(\eta))}
   for @math{\eta >= 0}. In the flat limit this takes the form
   @math{L^@{H3d@}_0(\lambda,\eta) = j_0(\lambda\eta)}."
  :return (sf-result))

(defun-gsl legendre-H3d-1 ((lambda :double) (eta :double))
  "gsl_sf_legendre_H3d_1_e"
  :documentation "The first radial eigenfunction of the Laplacian on
   the 3-dimensional hyperbolic space,
   @math{L^@{H3d@}_1(\lambda,\eta) := 1/\sqrt@{\lambda^2 + 1@}
   \sin(\lambda \eta)/(\lambda \sinh(\eta)) (\coth(\eta) - \lambda \cot(\lambda\eta))}
   for @math{\eta >= 0}.  In the flat limit this takes the form 
   @math{L^@{H3d@}_1(\lambda,\eta) = j_1(\lambda\eta)}."
  :return (sf-result))

(defun-gsl legendre-H3d ((l :int) (lambda :double) (eta :double))
  "gsl_sf_legendre_H3d_e"
  :documentation "The @var{l}-th radial eigenfunction of the
   Laplacian on the 3-dimensional hyperbolic space
   @math{\eta >= 0}, @c{$l \ge 0$} @math{l >= 0}.
   In the flat limit this takes the form
   @math{L^@{H3d@}_l(\lambda,\eta) = j_l(\lambda\eta)}."
  :return (sf-result))

(defun-gsl legendre-H3d-array
    (((dim0 array) :int) (lambda :double) (eta :double)
     ((gsl-array array) :pointer))
  "gsl_sf_legendre_H3d_array"
  :documentation "An array of radial eigenfunctions
   @math{L^@{H3d@}_l(\lambda, \eta)} for @math{0 <= l <= lmax}."
  :function (lambda eta array)
  :invalidate (array)
  :return-input (array))

;;; (defparameter hleg (make-data 'vector nil 3))
;;; (legendre-H3d-array 1.0d0 0.5d0 hleg)
;;; #<GSL-VECTOR #(0.9200342692589383d0 0.21694026450392123d0 0.047950660488307775d0) {C07CB51}>

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test legendre
  (lisp-unit:assert-first-fp-equal
   "-0.365000000000d+00"
   (legendre-P2 0.3d0))
  (lisp-unit:assert-error 'gsl-error (legendre-Pl -4 0.3d0))
  (lisp-unit:assert-error 'gsl-error (legendre-Pl 4 3.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.729375000000d-01"
   (legendre-Pl 4 0.3d0))
  (lisp-unit:assert-first-fp-equal
   "0.312852949882d+00"
   (legendre-Q0 3.3d0))
  (lisp-unit:assert-first-fp-equal
   "-0.340997502740d+02"
   (legendre-plm 4 3 0.5d0))
  (lisp-unit:assert-first-fp-equal
   "0.398506257222d-13"
   (legendre-sphplm 1200 1100 0.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.125529904888d+00"
   (LEGENDRE-CONICALP-HALF 3.5d0 10.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.627433629279d-01"
   (LEGENDRE-CONICALP-MHALF 3.5d0 10.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "-0.131636618937d+00"
   (LEGENDRE-CONICALP-0 3.5d0 10.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.174071955601d+00"
   (LEGENDRE-CONICALP-1 3.5d0 10.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.898079795297d-03"
   (LEGENDRE-REGULAR-SPHERICAL-CONICAL 3 3.5d0 10.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.230602506199d-02"
   (LEGENDRE-REGULAR-CYLINDRICAL-CONICAL 3 3.5d0 10.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.920034269259d+00"
   (LEGENDRE-H3D-0 1.0d0 0.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.216940264504d+00"
   (LEGENDRE-H3D-1 1.0d0 0.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL
   "0.240061623900d-02"
   (LEGENDRE-H3D 4 1.0d0 0.5d0)))
