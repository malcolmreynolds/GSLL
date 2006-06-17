;********************************************************
; file:        legendre.lisp                             
; description: Legendre functions                        
; date:        Sat Apr 29 2006 - 19:16                   
; author:      Liam M. Healy                             
; modified:    Fri Jun 16 2006 - 22:56
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; legendre-Plm-deriv-array same answer as legendre-Plm-array?

;;;;****************************************************************************
;;;; Legendre polynomials
;;;;****************************************************************************

(defun-gsl legendre-P1 (x)
  "gsl_sf_legendre_P1_e" ((x :double) (ret sf-result))
  :documentation
  "The Legendre polynomials @math{P_1(x)} using an explicit
   representation.")

(defun-gsl legendre-P2 (x)
  "gsl_sf_legendre_P2_e" ((x :double) (ret sf-result))
  :documentation
  "The Legendre polynomials @math{P_2(x)} using an explicit
   representation.")

(defun-gsl legendre-P3 (x)
  "gsl_sf_legendre_P3_e" ((x :double) (ret sf-result))
  :documentation
  "The Legendre polynomials @math{P_3(x)} using an explicit
   representation.")

(defun-gsl legendre-Pl (l x)
  "gsl_sf_legendre_Pl_e" ((l :int) (x :double) (ret sf-result))
  :documentation
  "The Legendre polynomial @math{P_l(x)} for a specific value of @var{l},
   @var{x} subject to @math{l >= 0}, @math{|x| <= 1}.")

(defun-gsl legendre-Pl-array (x array)
  "gsl_sf_legendre_Pl_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :documentation "Compute an array of Legendre polynomials
  @math{P_l(x)} for @math{l = 0, \dots, length(array)}, @math{|x| <= 1}."
  :invalidate (array))

(defun-gsl legendre-Pl-deriv-array (x array)
  "gsl_sf_legendre_Pl_deriv_array"
  (((1- (dim0 array)) :int) (x :double) ((gsl-array array) :pointer))
  :documentation "Compute an array of Legendre polynomials derivatives
  @math{dP_l(x)/dx}, for @math{l = 0, \dots,  length(array)}, @math{|x| <= 1}."
  :invalidate (array))

(defun-gsl legendre-Q0 (x)
  "gsl_sf_legendre_Q0_e" ((x :double) (ret sf-result))
  :documentation
  "The Legendre function @math{Q_0(x)} for @math{x > -1},
   @math{x /= 1}.")

(defun-gsl legendre-Q1 (x)
  "gsl_sf_legendre_Q1_e" ((x :double) (ret sf-result))
  :documentation
  "The Legendre function @math{Q_1(x)} for @math{x > -1},
   @math{x /= 1}.")

(defun-gsl legendre-Ql (l x)
  "gsl_sf_legendre_Ql_e" ((l :int) (x :double) (ret sf-result))
  :documentation
  "The Legendre function @math{Q_l(x)} for @math{x > -1},
   @math{x /= 1}, @math{l >= 0}.")

;;;;****************************************************************************
;;;; Associated Legendre Polynomials and Spherical Harmonics
;;;;****************************************************************************

;;; The following functions compute the associated Legendre Polynomials
;;; @math{P_l^m(x)}.  Note that this function grows combinatorially with
;;; @math{l} and can overflow for @math{l} larger than about 150.  There is
;;; no trouble for small @math{m}, but overflow occurs when @math{m} and
;;; @math{l} are both large.  Rather than allow overflows, these functions
;;; refuse to calculate @math{P_l^m(x)} and return :EOVRFLW when
;;; they can sense that @math{l} and @math{m} are too big.

;;; If you want to calculate a spherical harmonic, then @emph{do not} use
;;; these functions.  Instead use legendre-sphPlm below,
;;; which uses a similar recursion, but with the normalized functions.

(defun-gsl legendre-Plm (l m x)
  "gsl_sf_legendre_Plm_e" ((l :int) (m :int) (x :double) (ret sf-result))
  :documentation
  "The associated Legendre polynomial
   @math{P_l^m(x)} for @math{m >= 0}, @math{l >= m}, @math{|x| <= 1}.")

(defun-gsl legendre-Plm-array (m x array)
  "gsl_sf_legendre_Plm_array"
  (((+ (dim0 array) m -1) :int) (m :int) (x :double)
   ((gsl-array array) :pointer))
  :documentation "An array of Legendre polynomials
    @math{P_l^m(x)}, for @math{m >= 0}, 
    @math{l = |m|, ..., |m|+length(array)-1} and @math{|x| <= 1}."
  :invalidate (array))

(defun-gsl legendre-Plm-deriv-array (m x array)
  "gsl_sf_legendre_Plm_deriv_array"
  (((+ (dim0 array) m -1) :int) (m :int) (x :double)
   ((gsl-array array) :pointer))
  :documentation "An array of Legendre polynomials derivatives
    @math{dP_l^m(x)/dx} for @math{m >= 0}, 
    @math{l = |m|, ..., length(array)} and @math{|x| <= 1}."
  :invalidate (array))

(defun-gsl legendre-sphPlm (l m x)
  "gsl_sf_legendre_sphPlm_e" ((l :int) (m :int) (x :double) (ret sf-result))
  :documentation
  "The normalized associated Legendre polynomial
   @math{$\sqrt@{(2l+1)/(4\pi)@} \sqrt@{(l-m)!/(l+m)!@} P_l^m(x)$} suitable
   for use in spherical harmonics.  The parameters must satisfy
   @math{m >= 0}, @math{l >= m}, @math{|x| <= 1}.
   These routines avoid the overflows that occur for the standard
   normalization of @math{P_l^m(x)}.")

(defun-gsl legendre-sphPlm-array (m x array)
  "gsl_sf_legendre_sphPlm_array"
  (((+ (dim0 array) m -1) :int) (m :int) (x :double)
   ((gsl-array array) :pointer))
  :documentation "An array of normalized associated Legendre functions
   @math{$\sqrt@{(2l+1)/(4\pi)@} \sqrt@{(l-m)!/(l+m)!@} P_l^m(x)$},
   for @math{m >= 0}, @math{l = |m|, ..., length(array)}, @math{|x| <= 1.0}."
  :invalidate (array))

(defun-gsl legendre-sphPlm-deriv-array (m x array)
  "gsl_sf_legendre_sphPlm_deriv_array"
  (((+ (dim0 array) m -1) :int) (m :int) (x :double)
   ((gsl-array array) :pointer))
  :documentation "An array of normalized associated Legendre functions
   derivatives for @math{m >= 0}, @math{l = |m|, ..., length(array)},
   @math{|x| <= 1.0}."
  :invalidate (array))

(defun-gsl legendre-array-size (lmax m)
  "gsl_sf_legendre_array_size" ((lmax :int) (m :int))
  :documentation "The size of @var{result_array}[] needed for the array
    versions of @math{P_l^m(x)}, @math{@var{lmax} - @var{m} + 1}."
  :c-return :int)

;;;;****************************************************************************
;;;; Conical Functions
;;;;****************************************************************************

;;; The Conical Functions @math{P^\mu_@{-(1/2)+i\lambda@}(x)} and 
;;; @math{Q^\mu_@{-(1/2)+i\lambda@}} 
;;; are described in Abramowitz & Stegun, Section 8.12.

(defun-gsl legendre-conicalP-half (lambda x)
  "gsl_sf_conicalP_half_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation "The irregular Spherical Conical Function
   @math{P^@{1/2@}_@{-1/2 + i \lambda@}(x)} for @math{x > -1}.")

(defun-gsl legendre-conicalP-mhalf (lambda x)
  "gsl_sf_conicalP_mhalf_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation "The regular Spherical Conical Function
  @math{P^@{-1/2@}_@{-1/2 + i \lambda@}(x)} for @math{x > -1}.")

(defun-gsl legendre-conicalP-0 (lambda x)
  "gsl_sf_conicalP_0_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation "The conical function @math{P^0_@{-1/2 + i \lambda@}(x)}
  for @math{x > -1}.")

(defun-gsl legendre-conicalP-1 (lambda x)
  "gsl_sf_conicalP_1_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation "The conical function 
   @math{P^1_@{-1/2 + i \lambda@}(x)} for @math{x > -1}.")

(defun-gsl legendre-regular-spherical-conical (l lambda x)
  "gsl_sf_conicalP_sph_reg_e"
  ((l :int) (lambda :double) (x :double) (ret sf-result))
  :documentation "The Regular Spherical Conical Function
   @math{P^@{-1/2-l@}_@{-1/2 + i \lambda@}(x)} for @math{x > -1},
   @math{l >= -1}.")

(defun-gsl legendre-regular-cylindrical-conical (l lambda x)
  "gsl_sf_conicalP_cyl_reg_e"
  ((l :int) (lambda :double) (x :double) (ret sf-result))
  :documentation "The Regular Cylindrical Conical Function
   @math{P^@{-m@}_@{-1/2 + i \lambda@}(x)} for @math{x > -1},
   @math{m >= -1}.")

;;;;****************************************************************************
;;;; Radial Functions for Hyperbolic Space
;;;;****************************************************************************

;;; The following spherical functions are specializations of Legendre
;;; functions which give the regular eigenfunctions of the Laplacian on a
;;; 3-dimensional hyperbolic space @math{H3d}.  Of particular interest is
;;; the flat limit, @math{\lambda \to \infty}, @math{\eta \to 0},
;;; @math{\lambda\eta} fixed.

(defun-gsl legendre-H3d-0 (lambda eta)
  "gsl_sf_legendre_H3d_0_e"
  ((lambda :double) (eta :double) (ret sf-result))
  :documentation "The zeroth radial eigenfunction of the Laplacian on the
   3-dimensional hyperbolic space,
   @math{L^@{H3d@}_0(\lambda,\eta) := \sin(\lambda\eta)/(\lambda\sinh(\eta))}
   for @math{\eta >= 0}. In the flat limit this takes the form
   @math{L^@{H3d@}_0(\lambda,\eta) = j_0(\lambda\eta)}.")

(defun-gsl legendre-H3d-1 (lambda eta)
  "gsl_sf_legendre_H3d_1_e"
  ((lambda :double) (eta :double) (ret sf-result))
  :documentation "The first radial eigenfunction of the Laplacian on
   the 3-dimensional hyperbolic space,
   @math{L^@{H3d@}_1(\lambda,\eta) := 1/\sqrt@{\lambda^2 + 1@}
   \sin(\lambda \eta)/(\lambda \sinh(\eta)) (\coth(\eta) - \lambda \cot(\lambda\eta))}
   for @math{\eta >= 0}.  In the flat limit this takes the form 
   @math{L^@{H3d@}_1(\lambda,\eta) = j_1(\lambda\eta)}.")

(defun-gsl legendre-H3d (l lambda eta)
  "gsl_sf_legendre_H3d_e"
  ((l :int) (lambda :double) (eta :double) (ret sf-result))
  :documentation "The @var{l}-th radial eigenfunction of the
   Laplacian on the 3-dimensional hyperbolic space
   @math{\eta >= 0}, @c{$l \ge 0$} @math{l >= 0}.
   In the flat limit this takes the form
   @math{L^@{H3d@}_l(\lambda,\eta) = j_l(\lambda\eta)}.")

(defun-gsl legendre-H3d-array (lambda eta array)
  "gsl_sf_legendre_H3d_array"
  (((1- (dim0 array)) :int) (lambda :double) (eta :double)
   ((gsl-array array) :pointer))
  :documentation "An array of radial eigenfunctions
   @math{L^@{H3d@}_l(\lambda, \eta)} for @math{0 <= l <= length(array)}."
  :invalidate (array))

;;; (defparameter hleg (make-data 'vector nil 3))
;;; (legendre-H3d-array 1.0d0 0.5d0 hleg)
;;; #<GSL-VECTOR #(0.9200342692589383d0 0.21694026450392123d0 0.047950660488307775d0) {C07CB51}>

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test legendre
  (lisp-unit:assert-first-fp-equal
   "0.300000000000d+00"
   (legendre-P1 0.3d0))
  (lisp-unit:assert-first-fp-equal
   "-0.365000000000d+00"
   (legendre-P2 0.3d0))
  (lisp-unit:assert-first-fp-equal
   "-0.382500000000d+00"
   (legendre-P3 0.3d0))
  (lisp-unit:assert-error 'gsl-error (legendre-Pl -4 0.3d0))
  (lisp-unit:assert-error 'gsl-error (legendre-Pl 4 3.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.729375000000d-01"
   (legendre-Pl 4 0.3d0))
  (lisp-unit:assert-equal
   '("0.100000000000d+01" "0.500000000000d+00"
     "-0.125000000000d+00" "-0.437500000000d+00")
   (lisp-unit:fp-sequence
    (with-data (arr vector-double 4)
      (legendre-Pl-array 0.5d0 arr)
      (data arr))))
  (lisp-unit:assert-first-fp-equal
   "0.312852949882d+00"
   (legendre-Q0 3.3d0))
  (lisp-unit:assert-first-fp-equal
   "0.324147346113d-01"
   (legendre-Q1 3.3d0))
  (lisp-unit:assert-first-fp-equal
   "0.402646138474d-02"
   (legendre-Ql 2 3.3d0))
  (lisp-unit:assert-first-fp-equal
   "-0.340997502740d+02"
   (legendre-Plm 4 3 0.5d0))
  (lisp-unit:assert-equal
   '("0.225000000000d+01" "0.562500000000d+01"
     "0.421875000000d+01" "-0.492187500000d+01")
   (lisp-unit:fp-sequence
    (with-data (arr vector-double 4)
      (legendre-Plm-array 2 0.5d0 arr)
      (data arr))))
  (lisp-unit:assert-equal
   '("0.225000000000d+01" "0.562500000000d+01"
     "0.421875000000d+01" "-0.492187500000d+01")
   ;; suspicious? same answer as legendre-Plm-array?
   (lisp-unit:fp-sequence
    (with-data (arr vector-double 4)
      (legendre-Plm-deriv-array 2 0.5d0 arr)
      (data arr))))
  (lisp-unit:assert-first-fp-equal
   "0.398506257222d-13"
   (legendre-sphplm 1200 1100 0.5d0))
  (lisp-unit:assert-equal
   '("0.248924639500d+00" "0.412794815148d+00"
     "0.351206555622d+00" "0.515993518936d-01")
   (lisp-unit:fp-sequence
    (with-data (arr vector-double 4)
      (legendre-sphPlm-array 4 0.5d0 arr)
      (data arr))))
  ;; suspicious? same answer as legendre-sphPlm-array?
  (lisp-unit:assert-equal
   '("0.248924639500d+00" "0.412794815148d+00"
     "0.351206555622d+00" "0.515993518936d-01")
   (lisp-unit:fp-sequence
    (with-data (arr vector-double 4)
      (legendre-sphPlm-deriv-array 4 0.5d0 arr)
      (data arr))))
  (lisp-unit:assert-first-fp-equal
   "-0.125529904888d+00"
   (legendre-conicalp-half 3.5d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "-0.627433629279d-01"
   (legendre-conicalp-mhalf 3.5d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "-0.131636618937d+00"
   (legendre-conicalp-0 3.5d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.174071955601d+00"
   (legendre-conicalp-1 3.5d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.898079795297d-03"
   (legendre-regular-spherical-conical 3 3.5d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.230602506199d-02"
   (legendre-regular-cylindrical-conical 3 3.5d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.920034269259d+00"
   (legendre-h3d-0 1.0d0 0.5d0))
  (lisp-unit:assert-first-fp-equal
   "0.216940264504d+00"
   (legendre-h3d-1 1.0d0 0.5d0))
  (lisp-unit:assert-first-fp-equal
   "0.240061623900d-02"
   (legendre-h3d 4 1.0d0 0.5d0))
  (lisp-unit:assert-equal
   '("0.920034269259d+00" "0.216940264504d+00"
     "0.479506604883d-01" "0.106637690961d-01")
   (lisp-unit:fp-sequence
    (with-data (arr vector-double 4)
      (legendre-h3d-array 1.0d0 0.5d0 arr)
      (data arr)))))
