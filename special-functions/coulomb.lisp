;********************************************************
; file:        coulumb.lisp                              
; description: Coulumb functions                         
; date:        Sat Mar 18 2006 - 23:23                   
; author:      Liam M. Healy                             
; modified:    Sun May 21 2006 - 14:50
;********************************************************
;;; $Id:$

(in-package :gsl)

;;;;****************************************************************************
;;;; Normalized Hydrogenic Bound States
;;;;****************************************************************************

(defun-gsl hydrogenicR-1 ((x :double) (r :double))
  "gsl_sf_hydrogenicR_1_e"
  :documentation
  "The lowest-order normalized hydrogenic bound state radial wavefunction @c{$R_1 := 2Z \sqrt{Z} \exp(-Z r)$} @math{R_1 := 2Z \sqrt@{Z@} \exp(-Z r)}."
  :return (sf-result))

(defun-gsl hydrogenicR ((n :int) (l :int) (x :double) (r :double))
  "gsl_sf_hydrogenicR_e"
  :documentation
  "The @var{n}-th normalized hydrogenic bound state radial wavefunction,
$$R_n := {2 Z^{3/2} \over n^2}  \left({2Z \over n}\right)^l  \sqrt{(n-l-1)! \over (n+l)!} \exp(-Z r/n) L^{2l+1}_{n-l-1}(2Z/n r).
$$
The normalization is chosen such that the wavefunction @math{\psi} is given by 
@c{$\psi(n,l,r) = R_n Y_{lm}$}
@math{\psi(n,l,r) = R_n Y_@{lm@}}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Coulomb Wave Functions
;;;;****************************************************************************

;;; Comments are direct from GSL and aren't lispized yet.

;;; Returns should be reorganized sensibly.
(defun-gsl coulomb-wave-FG ((eta :double) (x :double) (L-F :double) (k :int))
  "gsl_sf_coulomb_wave_FG_e"
  :documentation
  "The Coulomb wave functions @math{F_L(\eta,x)},
@c{$G_{L-k}(\eta,x)$} 
@math{G_@{L-k@}(\eta,x)} and their derivatives 
@math{F'_L(\eta,x)}, 
@c{$G'_{L-k}(\eta,x)$}
@math{G'_@{L-k@}(\eta,x)}
with respect to @math{x}.  The parameters are restricted to @math{L,
L-k > -1/2}, @math{x > 0} and integer @math{k}.  Note that @math{L}
itself is not restricted to being an integer. The results are stored in
the parameters @var{F}, @var{G} for the function values and @var{Fp},
@var{Gp} for the derivative values.  If an overflow occurs,
@code{GSL_EOVRFLW} is returned and scaling exponents are stored in
the modifiable parameters @var{exp_F}, @var{exp_G}."
  :return
  (sf-result sf-result sf-result sf-result :double :double))

(defun-gsl coulomb-wave-F-array
    ((L-min :double) (kmax :int) (eta :double) (x :double))
  "gsl_sf_coulomb_wave_F_array"
  :documentation
  "The Coulomb wave function @math{F_L(\eta,x)} for
@math{L = Lmin \dots Lmin + kmax}, storing the results in @var{fc_array}.
In the case of overflow the exponent is stored in @var{F_exponent}."
  :return ((:double (1+ kmax)) :double))

(defun-gsl coulomb-wave-FG-array
    ((L-min :double) (kmax :int) (eta :double) (x :double))
  "gsl_sf_coulomb_wave_F_array"
  :documentation
  "The functions @math{F_L(\eta,x)},
@math{G_L(\eta,x)} for @math{L = Lmin \dots Lmin + kmax} storing the
results in @var{fc_array} and @var{gc_array}.  In the case of overflow the
exponents are stored in @var{F_exponent} and @var{G_exponent}."
  :return ((:double (1+ kmax)) (:double (1+ kmax)) :double :double))

(defun-gsl coulomb-wave-FGp-array 
    ((L-min :double) (kmax :int) (eta :double) (x :double))
  "gsl_sf_coulomb_wave_FGp_array"
  :documentation
  "The functions @math{F_L(\eta,x)},
@math{G_L(\eta,x)} and their derivatives @math{F'_L(\eta,x)},
@math{G'_L(\eta,x)} for @math{L = Lmin \dots Lmin + kmax} storing the
results in @var{fc_array}, @var{gc_array}, @var{fcp_array} and @var{gcp_array}.
In the case of overflow the exponents are stored in @var{F_exponent} 
and @var{G_exponent}."
  :return ((:double (1+ kmax)) (:double (1+ kmax))
	   (:double (1+ kmax)) (:double (1+ kmax))
	   :double :double))

(defun-gsl coulomb-wave-sphF-array
    ((L-min :double) (kmax :int) (eta :double) (x :double))
  "gsl_sf_coulomb_wave_sphF_array"
  :documentation
  "The Coulomb wave function divided by the argument
@math{F_L(\eta, x)/x} for @math{L = Lmin \dots Lmin + kmax}, storing the
results in @var{fc_array}.  In the case of overflow the exponent is
stored in @var{F_exponent}. This function reduces to spherical Bessel
functions in the limit @math{\eta \to 0}."
  :return ((:double (1+ kmax)) :double :double))

;;;;****************************************************************************
;;;; Coulomb Wave Function Normalization Constant
;;;;****************************************************************************

(defun-gsl coulomb-CL ((L :double) (eta :double))
  "gsl_sf_coulomb_CL_e"
  :documentation
  "The Coulomb wave function normalization constant @math{C_L(\eta)} for @math{L > -1}."
  :return (sf-result))

(defun-gsl coulomb-CL-array ((Lmin :double) (kmax :int) (eta :double))
  "gsl_sf_coulomb_CL_array"
  :documentation
  "The Coulomb wave function normalization constant @math{C_L(\eta)} for @math{L = Lmin \dots Lmin + kmax}, @math{Lmin > -1}."
  :return ((:double (1+ kmax))))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test coulomb
  (lisp-unit:assert-first-fp-equal "0.164169997248d+00"
				   (hydrogenicr-1 1.0d0 2.5d0))
  (lisp-unit:assert-first-fp-equal "0.766646808415d-01"
				   (hydrogenicr 3 1 1.0d0 2.5d0))
  (lisp-unit:assert-first-fp-equal "0.716177996747d-01"
				   (coulomb-wave-fg 1.0d0 2.0d0 2.5d0 1))
  (lisp-unit:assert-first-fp-equal "0.138091464419d-02"
				   (coulomb-cl 1.0d0 2.5d0)))
