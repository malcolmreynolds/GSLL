;; Coulumb functions
;; Liam Healy, Sat Mar 18 2006 - 23:23
;; Time-stamp: <2008-09-14 22:29:18EDT coulomb.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Normalized Hydrogenic Bound States
;;;;****************************************************************************

(defmfun hydrogenicR-1 (x r)
  "gsl_sf_hydrogenicR_1_e" ((x :double) (r :double) (ret sf-result))
  :documentation			; FDL
  "The lowest-order normalized hydrogenic bound state radial
   wavefunction R_1 := 2Z \sqrt{Z} \exp(-Z r).")

(defmfun hydrogenicR (n l x r)
  "gsl_sf_hydrogenicR_e"
  ((n :int) (l :int) (x :double) (r :double) (ret sf-result))
  :documentation			; FDL
  "The n-th normalized hydrogenic bound state radial wavefunction,
  R_n := {2 Z^{3/2} \over n^2}  \left({2Z \over n}\right)^l
  \sqrt{(n-l-1)! \over (n+l)!} \exp(-Z r/n) L^{2l+1}_{n-l-1}(2Z/n r).
  The normalization is chosen such that the wavefunction \psi is given by 
  \psi(n,l,r) = R_n Y_{lm}.")

;;;;****************************************************************************
;;;; Coulomb Wave Functions
;;;;****************************************************************************

;;; Comments are direct from GSL and aren't lispized yet.

(defmfun coulomb-wave-FG (eta x L-F k)
  "gsl_sf_coulomb_wave_FG_e"
  ((eta :double) (x :double) (L-F :double) (k :int)
   (F sf-result) (Fp sf-result) (G sf-result) (Gp sf-result)
   (exp-F :double) (exp-G :double))
  :return
  ((val F) (val Fp) (val G) (val Gp)
   (dcref exp-F) (dcref exp-G)
   (err F) (err Fp) (err G) (err Gp))
  :documentation			; FDL
  "The Coulomb wave functions F_L(\eta,x),
  G_{L-k}(\eta,x) and their derivatives F'_L(\eta,x), 
  G'_{L-k}(\eta,x) with respect to x.  The parameters are restricted to
  L, L-k > -1/2}, x > 0 and integer k.  Note that L
  itself is not restricted to being an integer. The results are stored in
  the parameters F, G for the function values and Fp,
  Gp for the derivative values.  If an overflow occurs,
  :EOVRFLW is signalled and scaling exponents are stored in
  the modifiable parameters exp-F, exp-G.")

(defmfun coulomb-wave-F-array (L-min eta x fc-array)
  "gsl_sf_coulomb_wave_F_array"
  ((L-min :double) ((1- (dim0 fc-array)) :int) (eta :double) (x :double)
   ((c-pointer fc-array) :pointer) (F-exponent :double))
  :outputs (fc-array)
  :return (fc-array (dcref F-exponent))
  :documentation			; FDL
  "The Coulomb wave function F_L(\eta,x) for
  L = Lmin ... Lmin + kmax, storing the results in fc-array.
  In the case of overflow the exponent is stored in the second value returned.")

(defmfun coulomb-wave-FG-array (L-min eta x fc-array gc-array)
  "gsl_sf_coulomb_wave_FG_array"
  ((L-min :double) ((1- (dim0 fc-array)) :int) (eta :double) (x :double)
   ((c-pointer fc-array) :pointer) ((c-pointer gc-array) :pointer)
   (F-exponent :double) (G-exponent :double))
  :return (fc-array gc-array (dcref F-exponent) (dcref G-exponent))
  :documentation			; FDL
  "The functions F_L(\eta,x),
  G_L(\eta,x) for L = Lmin ... Lmin + kmax storing the
  results in fc_array and gc_array.  In the case of overflow the
  exponents are stored in F_exponent and G_exponent.")

(defmfun coulomb-wave-FGp-array (L-min eta x fc-array fcp-array gc-array gcp-array)
  "gsl_sf_coulomb_wave_FGp_array"
  ((L-min :double) ((1- (dim0 fc-array)) :int) (eta :double) (x :double)
   ((c-pointer fc-array) :pointer) ((c-pointer fcp-array) :pointer)
   ((c-pointer gc-array) :pointer) ((c-pointer gcp-array) :pointer)
   (F-exponent :double) (G-exponent :double))
  :return
  (fc-array fcp-array gc-array gcp-array
	    (dcref F-exponent) (dcref G-exponent))
  :documentation			; FDL
  "The functions F_L(\eta,x),
  G_L(\eta,x) and their derivatives F'_L(\eta,x),
  G'_L(\eta,x) for L = Lmin ... Lmin + kmax storing the
  results in fc_array, gc_array, fcp_array and gcp_array.
  In the case of overflow the exponents are stored in F_exponent
  and G_exponent.")

(defmfun coulomb-wave-sphF-array (L-min eta x fc-array)
  "gsl_sf_coulomb_wave_sphF_array"
  ((L-min :double) ((1- (dim0 fc-array)) :int) (eta :double) (x :double)
   ((c-pointer fc-array) :pointer) (F-exponent :double))
  :outputs (fc-array)
  :return (fc-array (dcref F-exponent))
  :documentation			; FDL
  "The Coulomb wave function divided by the argument
   F_L(\eta, x)/x for L = Lmin ... Lmin + kmax, storing the
   results in fc_array.  In the case of overflow the exponent is
   stored in F_exponent. This function reduces to spherical Bessel
   functions in the limit \eta \to 0.")

;;;;****************************************************************************
;;;; Coulomb Wave Function Normalization Constant
;;;;****************************************************************************

(defmfun coulomb-CL (L eta)
  "gsl_sf_coulomb_CL_e" ((L :double) (eta :double) (ret sf-result))
  :documentation			; FDL
  "The Coulomb wave function normalization constant C_L(\eta)
   for L > -1.")

(defmfun coulomb-CL-array (L-min eta cl)
  "gsl_sf_coulomb_CL_array"
  ((L-min :double) ((1- (dim0 cl)) :int) (eta :double)
   ((c-pointer cl) :pointer))
  :documentation			; FDL
  "The Coulomb wave function normalization constant C_L(\eta)
   for L = Lmin ... Lmin + kmax, Lmin > -1."
  :outputs (cl))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests coulomb
  (hydrogenicr-1 1.0d0 2.5d0)
  (hydrogenicr 3 1 1.0d0 2.5d0)
  (coulomb-wave-FG 0.0d0 1.0d0 2.0d0 0)
  (letm ((arr (vector-double-float 3)))
    (coulomb-wave-F-array 0.0d0 1.0d0 2.0d0 arr)
    (cl-array arr))
  (coulomb-wave-fg 1.0d0 2.0d0 2.5d0 1)
  (letm ((Farr (vector-double-float 3)) (Garr (vector-double-float 3)))
    (coulomb-wave-FG-array 1.5d0 1.0d0 1.0d0 Farr Garr)
    (append (coerce (cl-array Farr) 'list) (coerce (cl-array Garr) 'list)))
  (letm ((arr (vector-double-float 3)))
    (coulomb-wave-sphF-array  0.0d0 1.0d0 2.0d0 arr) (cl-array arr))
  (coulomb-cl 1.0d0 2.5d0)
  (letm ((cl (vector-double-float 3)))
    (coulomb-CL-array 0.0d0 1.0d0 cl) (cl-array cl)))
|#

#|
(LISP-UNIT:DEFINE-TEST COULOMB
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1641699972477976d0 1.8226531089715347d-16)
   (MULTIPLE-VALUE-LIST (HYDROGENICR-1 1.0d0 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.07666468084146327d0 4.203054519905891d-16)
   (MULTIPLE-VALUE-LIST (HYDROGENICR 3 1 1.0d0 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.06203505201137388d0 0.17709857491700906d0
	 3.6050175661599693d0 -5.8282618416439025d0 0.0d0
	 0.0d0 4.3793707124032857d-16
	 1.2502291640824433d-15 3.5328525523867457d-14
	 5.711592064490999d-14)
   (MULTIPLE-VALUE-LIST
    (COULOMB-WAVE-FG 0.0d0 1.0d0 2.0d0 0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.6617816138326813d0 0.3614128577450535d0
      0.13267757609917497d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((ARR (VECTOR-DOUBLE-FLOAT 3)))
      (COULOMB-WAVE-F-ARRAY 0.0d0 1.0d0 2.0d0 ARR)
      (CL-ARRAY ARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.07161779967468254d0 0.1278101031568499d0
	 2.2510703464871114d0 -1.4245543587641651d0 0.0d0
	 0.0d0 8.269219937869759d-15
	 1.4757362807662922d-14 2.5991577338697564d-13
	 1.644835970887306d-13)
   (MULTIPLE-VALUE-LIST
    (COULOMB-WAVE-FG 1.0d0 2.0d0 2.5d0 1)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.035021584603913254d0 0.005752500614202263d0
	  7.116955601984411d-4 6.471726496134135d0
	  27.57457472159366d0 170.56037293106908d0))
   (MULTIPLE-VALUE-LIST
    (LETM
	((FARR (VECTOR-DOUBLE-FLOAT 3)) (GARR (VECTOR-DOUBLE-FLOAT 3)))
      (COULOMB-WAVE-FG-ARRAY 1.5d0 1.0d0 1.0d0 FARR GARR)
      (APPEND (COERCE (CL-ARRAY FARR) 'LIST)
	      (COERCE (CL-ARRAY GARR) 'LIST)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.33089080691634065d0 0.18070642887252675d0
      0.06633878804958748d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((ARR (VECTOR-DOUBLE-FLOAT 3)))
      (COULOMB-WAVE-SPHF-ARRAY 0.0d0 1.0d0 2.0d0 ARR)
      (CL-ARRAY ARR))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0013809146441856027d0 2.759621819430441d-17)
   (MULTIPLE-VALUE-LIST (COULOMB-CL 1.0d0 2.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    #(0.10842251310207264d0 0.05111086283184191d0
      0.011428736368066591d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((CL (VECTOR-DOUBLE-FLOAT 3)))
      (COULOMB-CL-ARRAY 0.0d0 1.0d0 CL) (CL-ARRAY CL)))))
|#

