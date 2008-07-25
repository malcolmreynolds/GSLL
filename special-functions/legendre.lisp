;; Legendre functions
;; Liam Healy, Sat Apr 29 2006 - 19:16
;; Time-stamp: <2008-07-08 21:55:40EDT legendre.lisp>
;; $Id$

(in-package :gsl)

;;; legendre-Plm-deriv-array same answer as legendre-Plm-array?

;;;;****************************************************************************
;;;; Legendre polynomials
;;;;****************************************************************************

(defmfun legendre-P1 (x)
  "gsl_sf_legendre_P1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Legendre polynomials P_1(x) using an explicit
   representation.")

(defmfun legendre-P2 (x)
  "gsl_sf_legendre_P2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Legendre polynomials P_2(x) using an explicit
   representation.")

(defmfun legendre-P3 (x)
  "gsl_sf_legendre_P3_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Legendre polynomials P_3(x) using an explicit
   representation.")

(defmfun legendre-Pl (l x)
  "gsl_sf_legendre_Pl_e" ((l :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Legendre polynomial P_l(x) for a specific value of l,
   x subject to l >= 0, |x| <= 1.")

(defmfun legendre-Pl-array (x array)
  "gsl_sf_legendre_Pl_array"
  (((1- (dim0 array)) :int) (x :double) ((c-pointer array) :pointer))
  :documentation			; FDL
  "Compute an array of Legendre polynomials
  P_l(x) for l = 0, ..., length(array), |x| <= 1."
  :outputs (array))

(defmfun legendre-Pl-deriv-array (x array)
  "gsl_sf_legendre_Pl_deriv_array"
  (((1- (dim0 array)) :int) (x :double) ((c-pointer array) :pointer))
  :documentation			; FDL
  "Compute an array of Legendre polynomials derivatives
  dP_l(x)/dx, for l = 0, ...,  length(array), |x| <= 1."
  :outputs (array))

(defmfun legendre-Q0 (x)
  "gsl_sf_legendre_Q0_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Legendre function Q_0(x) for x > -1,
   x /= 1.")

(defmfun legendre-Q1 (x)
  "gsl_sf_legendre_Q1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Legendre function Q_1(x) for x > -1,
   x /= 1.")

(defmfun legendre-Ql (l x)
  "gsl_sf_legendre_Ql_e" ((l :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Legendre function Q_l(x) for x > -1, x /= 1, l >= 0.")

;;;;****************************************************************************
;;;; Associated Legendre Polynomials and Spherical Harmonics
;;;;****************************************************************************

;;; FDL
;;; The following functions compute the associated Legendre Polynomials
;;; P_l^m(x).  Note that this function grows combinatorially with
;;; l and can overflow for l larger than about 150.  There is
;;; no trouble for small m, but overflow occurs when m and
;;; l are both large.  Rather than allow overflows, these functions
;;; refuse to calculate P_l^m(x) and return :EOVRFLW when
;;; they can sense that l and m are too big.

;;; If you want to calculate a spherical harmonic, then do not use
;;; these functions.  Instead use legendre-sphPlm below,
;;; which uses a similar recursion, but with the normalized functions.

(defmfun legendre-Plm (l m x)
  "gsl_sf_legendre_Plm_e" ((l :int) (m :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The associated Legendre polynomial
   P_l^m(x) for m >= 0, l >= m, |x| <= 1.")

(defmfun legendre-Plm-array (m x array)
  "gsl_sf_legendre_Plm_array"
  (((+ (dim0 array) m -1) :int) (m :int) (x :double)
   ((c-pointer array) :pointer))
  :documentation			; FDL
  "An array of Legendre polynomials
    P_l^m(x), for m >= 0, 
    l = |m|, ..., |m|+length(array)-1} and |x| <= 1."
  :outputs (array))

(defmfun legendre-Plm-deriv-array (m x values derivatives)
  "gsl_sf_legendre_Plm_deriv_array"
  (((+ (dim0 values) m -1) :int) (m :int) (x :double)
   ((c-pointer values) :pointer) ((c-pointer derivatives) :pointer))
  :documentation			; FDL
  "An array of Legendre polynomials
    values and derivatives dP_l^m(x)/dx for m >= 0, 
    l = |m|, ..., length(values) and |x| <= 1."
  :outputs (values derivatives))

(defmfun legendre-sphPlm (l m x)
  "gsl_sf_legendre_sphPlm_e" ((l :int) (m :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "The normalized associated Legendre polynomial
   \sqrt{(2l+1)/(4\pi) \sqrt{(l-m)!/(l+m)!} P_l^m(x) suitable
   for use in spherical harmonics.  The parameters must satisfy
   m >= 0, l >= m, |x| <= 1.  These routines avoid the overflows
   that occur for the standard normalization of P_l^m(x).")

(defmfun legendre-sphPlm-array (m x array)
  "gsl_sf_legendre_sphPlm_array"
  (((+ (dim0 array) m -1) :int) (m :int) (x :double)
   ((c-pointer array) :pointer))
  :documentation			; FDL
  "An array of normalized associated Legendre functions
   \sqrt(2l+1)/(4\pi) \sqrt(l-m)!/(l+m)! P_l^m(x),
   for m >= 0, l = |m|, ..., length(array)}, |x| <= 1.0."
  :outputs (array))

(defmfun legendre-sphPlm-deriv-array (m x values derivatives)
  "gsl_sf_legendre_sphPlm_deriv_array"
  (((+ (dim0 values) m -1) :int) (m :int) (x :double)
   ((c-pointer values) :pointer) ((c-pointer derivatives) :pointer))
  :documentation			; FDL
  "An array of normalized associated Legendre functions
   values and derivatives for m >= 0,
   l = |m|, ..., length(array)}, |x| <= 1.0."
  :outputs (values derivatives))

(defmfun legendre-array-size (lmax m)
  "gsl_sf_legendre_array_size" ((lmax :int) (m :int))
  :documentation			; FDL
  "The size of result array needed for the array
   versions of P_l^m(x), lmax - m + 1."
  :c-return :int)

;;;;****************************************************************************
;;;; Conical Functions
;;;;****************************************************************************

;;; FDL
;;; The Conical Functions P^\mu_{-(1/2)+i\lambda}(x)} and 
;;; Q^\mu_{-(1/2)+i\lambda}
;;; are described in Abramowitz & Stegun, Section 8.12.

(defmfun legendre-conicalP-half (lambda x)
  "gsl_sf_conicalP_half_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The irregular Spherical Conical Function
   P^{1/2}_{-1/2 + i \lambda}(x) for x > -1.")

(defmfun legendre-conicalP-mhalf (lambda x)
  "gsl_sf_conicalP_mhalf_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The regular Spherical Conical Function
   P^{-1/2}_{-1/2 + i \lambda}(x) for x > -1.")

(defmfun legendre-conicalP-0 (lambda x)
  "gsl_sf_conicalP_0_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The conical function P^0_{-1/2 + i \lambda(x)} for x > -1.")

(defmfun legendre-conicalP-1 (lambda x)
  "gsl_sf_conicalP_1_e" ((lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The conical function 
   P^1_{-1/2 + i \lambda}(x)} for x > -1.")

(defmfun legendre-regular-spherical-conical (l lambda x)
  "gsl_sf_conicalP_sph_reg_e"
  ((l :int) (lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Regular Spherical Conical Function
   P^{-1/2-l}_{-1/2 + i \lambda}(x) for x > -1, l >= -1.")

(defmfun legendre-regular-cylindrical-conical (l lambda x)
  "gsl_sf_conicalP_cyl_reg_e"
  ((l :int) (lambda :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Regular Cylindrical Conical Function
   P^{-m}_{-1/2 + i \lambda}(x) for x > -1, m >= -1.")

;;;;****************************************************************************
;;;; Radial Functions for Hyperbolic Space
;;;;****************************************************************************

;;; FDL
;;; The following spherical functions are specializations of Legendre
;;; functions which give the regular eigenfunctions of the Laplacian
;;; on a 3-dimensional hyperbolic space H3d.  Of particular interest
;;; is the flat limit, \lambda \to \infty, \eta \to 0, \lambda\eta
;;; fixed.

(defmfun legendre-H3d-0 (lambda eta)
  "gsl_sf_legendre_H3d_0_e"
  ((lambda :double) (eta :double) (ret sf-result))
  :documentation			; FDL
  "The zeroth radial eigenfunction of the Laplacian on the
   3-dimensional hyperbolic space,
   L^{H3d}_0(\lambda,\eta) := \sin(\lambda\eta)/(\lambda\sinh(\eta))
   for \eta >= 0. In the flat limit this takes the form
   L^{H3d}_0(\lambda,\eta) = j_0(\lambda\eta).")

(defmfun legendre-H3d-1 (lambda eta)
  "gsl_sf_legendre_H3d_1_e"
  ((lambda :double) (eta :double) (ret sf-result))
  :documentation			; FDL
  "The first radial eigenfunction of the Laplacian on
   the 3-dimensional hyperbolic space,
   L^{H3d}_1(\lambda,\eta) := 1/\sqrt{\lambda^2 + 1}
   \sin(\lambda \eta)/(\lambda \sinh(\eta)) (\coth(\eta) - \lambda \cot(\lambda\eta))}
   for \eta >= 0.  In the flat limit this takes the form 
   L^{H3d}_1(\lambda,\eta) = j_1(\lambda\eta)}.")

(defmfun legendre-H3d (l lambda eta)
  "gsl_sf_legendre_H3d_e"
  ((l :int) (lambda :double) (eta :double) (ret sf-result))
  :documentation			; FDL
  "The l-th radial eigenfunction of the
   Laplacian on the 3-dimensional hyperbolic space
   \eta >= 0, l >= 0.  In the flat limit this takes the form
   L^{H3d}_l(\lambda,\eta) = j_l(\lambda\eta).")

(defmfun legendre-H3d-array (lambda eta array)
  "gsl_sf_legendre_H3d_array"
  (((1- (dim0 array)) :int) (lambda :double) (eta :double)
   ((c-pointer array) :pointer))
  :outputs (array)
  :documentation			; FDL
  "An array of radial eigenfunctions
   L^{H3d}_l(\lambda, \eta) for 0 <= l <= length(array).")

;;; (defparameter hleg (make-data 'vector nil 3))
;;; (legendre-H3d-array 1.0d0 0.5d0 hleg)
;;; #<GSL-VECTOR #(0.9200342692589383d0 0.21694026450392123d0 0.047950660488307775d0) {C07CB51}>

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests legendre
  (legendre-P1 0.3d0)
  (legendre-P2 0.3d0)
  (legendre-P3 0.3d0)
  (legendre-Pl -4 0.3d0)
  (legendre-Pl 4 3.0d0)
  (legendre-Pl 4 0.3d0)
  (letm ((arr (vector-double-float 4)))
      (legendre-Pl-array 0.5d0 arr)
      (data arr))
  (legendre-Q0 3.3d0)
  (legendre-Q1 3.3d0)
  (legendre-Ql 2 3.3d0)
  (legendre-Plm 4 3 0.5d0)
  (letm ((arr (vector-double-float 4)))
      (legendre-Plm-array 2 0.5d0 arr)
      (data arr))
  (letm ((val (vector-double-float 4))
	   (deriv (vector-double-float 4)))
      (legendre-Plm-deriv-array 2 0.5d0 val deriv)
      (data deriv))
  (legendre-sphplm 1200 1100 0.3d0)
  (letm ((arr (vector-double-float 4)))
      (legendre-sphPlm-array 4 0.5d0 arr)
      (data arr))
  (letm ((val (vector-double-float 4))
	   (deriv (vector-double-float 4)))
	(legendre-sphPlm-deriv-array 4 0.5d0 val deriv)
	(data deriv))
  (legendre-conicalp-half 3.5d0 10.0d0)
  (legendre-conicalp-mhalf 3.5d0 10.0d0)
  (legendre-conicalp-0 3.5d0 10.0d0)
  (legendre-conicalp-1 3.5d0 10.0d0)
  (legendre-regular-spherical-conical 3 3.5d0 10.0d0)
  (legendre-regular-cylindrical-conical 3 3.5d0 10.0d0)
  (legendre-h3d-0 1.0d0 0.5d0)
  (legendre-h3d-1 1.0d0 0.5d0)
  (legendre-h3d 4 1.0d0 0.5d0)
  (letm ((arr (vector-double-float 4)))
      (legendre-h3d-array 1.0d0 0.5d0 arr)
      (data arr)))
|#

(LISP-UNIT:DEFINE-TEST LEGENDRE
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.3d0 0.0d0)
				     (MULTIPLE-VALUE-LIST
				      (LEGENDRE-P1
				       0.3d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.365d0 2.8199664825478977d-16)
   (MULTIPLE-VALUE-LIST (LEGENDRE-P2 0.3d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.38249999999999995d0 1.9984014443252816d-16)
   (MULTIPLE-VALUE-LIST (LEGENDRE-P3 0.3d0)))
  (LISP-UNIT:ASSERT-ERROR 'GSL-CONDITION
			  (LEGENDRE-PL -4 0.3d0))
  (LISP-UNIT:ASSERT-ERROR 'GSL-CONDITION
			  (LEGENDRE-PL 4 3.0d0))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.07293749999999999d0 5.668382430101814d-17)
   (MULTIPLE-VALUE-LIST (LEGENDRE-PL 4 0.3d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(1.0d0 0.5d0 -0.125d0 -0.4375d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((ARR (VECTOR-DOUBLE-FLOAT 4)))
      (LEGENDRE-PL-ARRAY 0.5d0 ARR) (DATA ARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3128529498822064d0 1.3893461931245028d-16)
   (MULTIPLE-VALUE-LIST (LEGENDRE-Q0 3.3d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.03241473461128108d0 1.4395033881023292d-17)
   (MULTIPLE-VALUE-LIST (LEGENDRE-Q1 3.3d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.004026461384737812d0 1.788108054840004d-18)
   (MULTIPLE-VALUE-LIST (LEGENDRE-QL 2 3.3d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -34.099750274012266d0 3.0286662310541114d-14)
   (MULTIPLE-VALUE-LIST (LEGENDRE-PLM 4 3 0.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(2.25d0 5.625d0 4.21875d0 -4.921875d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((ARR (VECTOR-DOUBLE-FLOAT 4)))
      (LEGENDRE-PLM-ARRAY 2 0.5d0 ARR) (DATA ARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #(-3.0d0 3.75d0 33.75d0 55.78125d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((VAL (VECTOR-DOUBLE-FLOAT 4)) (DERIV (VECTOR-DOUBLE-FLOAT 4)))
      (LEGENDRE-PLM-DERIV-ARRAY 2 0.5d0 VAL DERIV)
      (DATA DERIV))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.30366280894310793d0 3.438761110552081d-15)
   (MULTIPLE-VALUE-LIST
    (LEGENDRE-SPHPLM 1200 1100 0.3d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.24892463950030283d0 0.4127948151484927d0
      0.35120655562190445d0 0.051599351893561574d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((ARR (VECTOR-DOUBLE-FLOAT 4)))
      (LEGENDRE-SPHPLM-ARRAY 4 0.5d0 ARR)
      (DATA ARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(-0.6637990386674741d0 -0.2751965434323283d0
      1.2710332489173686d0 2.648766730536161d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((VAL (VECTOR-DOUBLE-FLOAT 4)) (DERIV (VECTOR-DOUBLE-FLOAT 4)))
      (LEGENDRE-SPHPLM-DERIV-ARRAY 4 0.5d0 VAL DERIV)
      (DATA DERIV))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.1255299048878925d0 1.3395992025650077d-15)
   (MULTIPLE-VALUE-LIST
    (LEGENDRE-CONICALP-HALF 3.5d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.06274336292793128d0 2.504525777730328d-16)
   (MULTIPLE-VALUE-LIST
    (LEGENDRE-CONICALP-MHALF 3.5d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.1316366189368757d0 3.0865532615549887d-15)
   (MULTIPLE-VALUE-LIST
    (LEGENDRE-CONICALP-0 3.5d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.17407195560076358d0 4.024312727070929d-15)
   (MULTIPLE-VALUE-LIST
    (LEGENDRE-CONICALP-1 3.5d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 8.980797952969897d-4 7.157154480497032d-17)
   (MULTIPLE-VALUE-LIST
    (LEGENDRE-REGULAR-SPHERICAL-CONICAL 3 3.5d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0023060250619859907d0 2.7345630541297237d-16)
   (MULTIPLE-VALUE-LIST
    (LEGENDRE-REGULAR-CYLINDRICAL-CONICAL 3 3.5d0
					  10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9200342692589382d0 1.7018240558144874d-15)
   (MULTIPLE-VALUE-LIST (LEGENDRE-H3D-0 1.0d0 0.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.2169402645039212d0 1.418962379417407d-15)
   (MULTIPLE-VALUE-LIST (LEGENDRE-H3D-1 1.0d0 0.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.002400616238997978d0 4.3381136468045d-17)
   (MULTIPLE-VALUE-LIST (LEGENDRE-H3D 4 1.0d0 0.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.9200342692589379d0 0.21694026450392115d0
      0.04795066048830776d0 0.010663769096144337d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((ARR (VECTOR-DOUBLE-FLOAT 4)))
      (LEGENDRE-H3D-ARRAY 1.0d0 0.5d0 ARR)
      (DATA ARR)))))
