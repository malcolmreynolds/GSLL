;********************************************************
; file:        coulumb.lisp                              
; description: Coulumb functions                         
; date:        Sat Mar 18 2006 - 23:23                   
; author:      Liam M. Healy                             
; modified:    Sat Mar  3 2007 - 22:45
;********************************************************
;;; $Id:$

(in-package :gsl)

;;;;****************************************************************************
;;;; Normalized Hydrogenic Bound States
;;;;****************************************************************************

(defun-gsl hydrogenicR-1 (x r)
  "gsl_sf_hydrogenicR_1_e" ((x :double) (r :double) (ret sf-result))
  :documentation
  "The lowest-order normalized hydrogenic bound state radial
   wavefunction @math{R_1 := 2Z \sqrt@{Z@} \exp(-Z r)}.")

(defun-gsl hydrogenicR (n l x r)
  "gsl_sf_hydrogenicR_e"
  ((n :int) (l :int) (x :double) (r :double) (ret sf-result))
  :documentation
  "The @var{n}-th normalized hydrogenic bound state radial wavefunction,
  R_n := {2 Z^{3/2} \over n^2}  \left({2Z \over n}\right)^l
  \sqrt{(n-l-1)! \over (n+l)!} \exp(-Z r/n) L^{2l+1}_{n-l-1}(2Z/n r).
  The normalization is chosen such that the wavefunction @math{\psi} is given by 
  @math{\psi(n,l,r) = R_n Y_@{lm@}}.")

;;;;****************************************************************************
;;;; Coulomb Wave Functions
;;;;****************************************************************************

;;; Comments are direct from GSL and aren't lispized yet.

(defun-gsl coulomb-wave-FG (eta x L-F k)
  "gsl_sf_coulomb_wave_FG_e"
  ((eta :double) (x :double) (L-F :double) (k :int)
   (F sf-result) (Fp sf-result) (G sf-result) (Gp sf-result)
   (exp-F :double) (exp-G :double))
  :return
  ((val F) (val Fp) (val G) (val Gp)
   (double-to-cl exp-F) (double-to-cl exp-G)
   (err F) (err Fp) (err G) (err Gp))
  :documentation
  "The Coulomb wave functions @math{F_L(\eta,x)},
  @math{G_@{L-k@}(\eta,x)} and their derivatives @math{F'_L(\eta,x)}, 
  @math{G'_@{L-k@}(\eta,x)}
  with respect to @math{x}.  The parameters are restricted to @math{L,
  L-k > -1/2}, @math{x > 0} and integer @math{k}.  Note that @math{L}
  itself is not restricted to being an integer. The results are stored in
  the parameters @var{F}, @var{G} for the function values and @var{Fp},
  @var{Gp} for the derivative values.  If an overflow occurs,
  :EOVRFLW is signalled and scaling exponents are stored in
  the modifiable parameters exp-F, exp-G.")

(defun-gsl coulomb-wave-F-array (L-min eta x fc-array)
  "gsl_sf_coulomb_wave_F_array"
  ((L-min :double) ((1- (dim0 fc-array)) :int) (eta :double) (x :double)
   ((gsl-array fc-array) :pointer) (F-exponent :double))
  :documentation
  "The Coulomb wave function @math{F_L(\eta,x)} for
  @math{L = Lmin \dots Lmin + kmax}, storing the results in @var{fc-array}.
  In the case of overflow the exponent is stored in the second value returned."
  :invalidate (fc-array)
  :return (fc-array (double-to-cl F-exponent)))

(defun-gsl coulomb-wave-FG-array (L-min eta x fc-array gc-array)
  "gsl_sf_coulomb_wave_FG_array"
  ((L-min :double) ((1- (dim0 fc-array)) :int) (eta :double) (x :double)
   ((gsl-array fc-array) :pointer) ((gsl-array gc-array) :pointer)
   (F-exponent :double) (G-exponent :double))
  :documentation
  "The functions @math{F_L(\eta,x)},
  @math{G_L(\eta,x)} for @math{L = Lmin \dots Lmin + kmax} storing the
  results in @var{fc_array} and @var{gc_array}.  In the case of overflow the
  exponents are stored in @var{F_exponent} and @var{G_exponent}."
  :return (fc-array gc-array (double-to-cl F-exponent) (double-to-cl G-exponent)))

(defun-gsl coulomb-wave-FGp-array (L-min eta x fc-array fcp-array gc-array gcp-array)
  "gsl_sf_coulomb_wave_FGp_array"
  ((L-min :double) ((1- (dim0 fc-array)) :int) (eta :double) (x :double)
   ((gsl-array fc-array) :pointer) ((gsl-array fcp-array) :pointer)
   ((gsl-array gc-array) :pointer) ((gsl-array gcp-array) :pointer)
   (F-exponent :double) (G-exponent :double))
  :documentation
  "The functions @math{F_L(\eta,x)},
  @math{G_L(\eta,x)} and their derivatives @math{F'_L(\eta,x)},
  @math{G'_L(\eta,x)} for @math{L = Lmin \dots Lmin + kmax} storing the
  results in @var{fc_array}, @var{gc_array}, @var{fcp_array} and @var{gcp_array}.
  In the case of overflow the exponents are stored in @var{F_exponent} 
  and @var{G_exponent}."
  :return
  (fc-array fcp-array gc-array gcp-array
	    (double-to-cl F-exponent) (double-to-cl G-exponent)))

(defun-gsl coulomb-wave-sphF-array (L-min eta x fc-array)
  "gsl_sf_coulomb_wave_sphF_array"
  ((L-min :double) ((1- (dim0 fc-array)) :int) (eta :double) (x :double)
   ((gsl-array fc-array) :pointer) (F-exponent :double))
  :documentation
  "The Coulomb wave function divided by the argument
   @math{F_L(\eta, x)/x} for @math{L = Lmin \dots Lmin + kmax}, storing the
   results in @var{fc_array}.  In the case of overflow the exponent is
   stored in @var{F_exponent}. This function reduces to spherical Bessel
   functions in the limit @math{\eta \to 0}."
  :invalidate (fc-array)
  :return (fc-array (double-to-cl F-exponent)))

;;;;****************************************************************************
;;;; Coulomb Wave Function Normalization Constant
;;;;****************************************************************************

(defun-gsl coulomb-CL (L eta)
  "gsl_sf_coulomb_CL_e" ((L :double) (eta :double) (ret sf-result))
  :documentation
  "The Coulomb wave function normalization constant @math{C_L(\eta)}
   for @math{L > -1}.")

(defun-gsl coulomb-CL-array (L-min eta cl)
  "gsl_sf_coulomb_CL_array"
  ((L-min :double) ((1- (dim0 cl)) :int) (eta :double)
   ((gsl-array cl) :pointer))
  :documentation
  "The Coulomb wave function normalization constant @math{C_L(\eta)}
   for @math{L = Lmin \dots Lmin + kmax}, @math{Lmin > -1}."
  :invalidate (cl))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test coulomb
  (lisp-unit:assert-first-fp-equal
   "0.164169997248d+00"
   (hydrogenicr-1 1.0d0 2.5d0))
  (lisp-unit:assert-first-fp-equal
   "0.766646808415d-01"
   (hydrogenicr 3 1 1.0d0 2.5d0))
  (lisp-unit:assert-equal
   '("0.620350520114d-01" "0.177098574917d+00" "0.360501756616d+01"
     "-0.582826184164d+01" "0.000000000000d+01" "0.000000000000d+01")
   (lisp-unit:fp-sequence
    (subseq (multiple-value-list (coulomb-wave-FG 0.0d0 1.0d0 2.0d0 0)) 0 6)))
  (lisp-unit:assert-equal
   '("0.661781613833d+00" "0.361412857745d+00" "0.132677576099d+00")
   (lisp-unit:fp-sequence
    (with-data (arr vector-double 3)
      (coulomb-wave-F-array  0.0d0 1.0d0 2.0d0 arr)
      (data arr))))
  (lisp-unit:assert-first-fp-equal
   "0.716177996747d-01"
   (coulomb-wave-fg 1.0d0 2.0d0 2.5d0 1))
  (lisp-unit:assert-equal
   '("0.350215846039d-01" "0.575250061420d-02" "0.711695560198d-03"
     "0.647172649613d+01" "0.275745747216d+02" "0.170560372931d+03")
   (lisp-unit:fp-sequence 
    (with-data (Farr vector-double 3)
      (with-data (Garr vector-double 3)
	(coulomb-wave-FG-array 1.5d0 1.0d0 1.0d0 Farr Garr)
	(append (coerce (data Farr) 'list) (coerce (data Garr) 'list))))))
  (lisp-unit:assert-equal
   '("0.330890806916d+00" "0.180706428873d+00" "0.663387880496d-01")
   (lisp-unit:fp-sequence
    (with-data (arr vector-double 3)
      (coulomb-wave-sphF-array  0.0d0 1.0d0 2.0d0 arr) (data arr))))
  (lisp-unit:assert-first-fp-equal
   "0.138091464419d-02"
   (coulomb-cl 1.0d0 2.5d0))
  (lisp-unit:assert-equal
   '("0.108422513102d+00" "0.511108628318d-01" "0.114287363681d-01")
   (lisp-unit:fp-sequence
    (with-data (cl vector-double 3)
      (coulomb-CL-array 0.0d0 1.0d0 cl) (data cl)))))
