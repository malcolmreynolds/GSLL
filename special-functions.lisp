;********************************************************
; file:        special-functions.lisp                                
; description: GSL types                                 
; date:        Sat Mar  4 2006 - 21:07                   
; author:      Liam M. Healy
; modified:    Sun Mar 12 2006 - 00:40
;********************************************************

(in-package :gsl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '(airy-ai airy-bi airy-ai-scaled airy-bi-scaled
     airy-ai-deriv airy-ai-deriv-scaled airy-bi-deriv airy-bi-deriv-scaled
     airy-zero-ai airy-zero-bi airy-zero-ai-deriv airy-zero-bi-deriv)))

;;;;****************************************************************************
;;;; Airy functions
;;;;****************************************************************************

(defun-sf airy-ai ((x :double))
  "gsl_sf_airy_Ai_e"
  :return (sf-result)
  :documentation "The Airy function Ai(x)."
  :mode t)

;;; (airy-ai 0.45d0)
;;; 0.24308135437540998d0
;;; 6.288670879282072d-17

(defun-sf airy-bi ((x :double))
  "gsl_sf_airy_Bi_e"
  :return (sf-result)
  :documentation "The Airy function Bi(x)."
  :mode t)

(defun-sf airy-ai-scaled ((x :double))
  "gsl_sf_airy_Ai_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function @math{S_A(x) Ai(x)}.  For @math{x>0} the scaling factor @math{S_A(x)} is @c{$\exp(+(2/3) x^{3/2})$} @math{\exp(+(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-sf airy-bi-scaled ((x :double))
  "gsl_sf_airy_Bi_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function @math{S_B(x) Bi(x)}.  For @math{x>0} the scaling factor @math{S_B(x)} is @c{$\exp(-(2/3) x^{3/2})$} @math{exp(-(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-sf airy-ai-deriv ((x :double))
  "gsl_sf_airy_Ai_deriv_e"
  :return (sf-result)
  :documentation "The Airy function derivative Ai'(x)."
  :mode t)

(defun-sf airy-bi-deriv ((x :double))
  "gsl_sf_airy_Bi_deriv_e"
  :return (sf-result)
  :documentation "The Airy function derivative Bi'(x)."
  :mode t)

(defun-sf airy-ai-deriv-scaled ((x :double))
  "gsl_sf_airy_Ai_deriv_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function derivative S_A(x) Ai'(x).  For @math{x>0} the scaling factor @math{S_A(x)} is @c{$\exp(+(2/3) x^{3/2})$} @math{\exp(+(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-sf airy-bi-deriv-scaled ((x :double))
  "gsl_sf_airy_Bi_deriv_scaled_e"
  :return (sf-result)
  :documentation "The scaled Airy function derivative S_B(x) Bi'(x).  For @math{x>0} the scaling factor @math{S_B(x)} is @c{$\exp(-(2/3) x^{3/2})$} @math{exp(-(2/3) x^(3/2))}, and is 1 for @math{x<0}."
  :mode t)

(defun-sf airy-zero-ai ((s :unsigned-int))
  "gsl_sf_airy_zero_Ai_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function @math{Ai(x)}."
  :mode t)

(defun-sf airy-zero-bi ((s :unsigned-int))
  "gsl_sf_airy_zero_Bi_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function @math{Bi(x)}."
  :mode t)

(defun-sf airy-zero-ai-deriv ((s :unsigned-int))
  "gsl_sf_airy_zero_Ai_deriv_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy
function derivative @math{Ai'(x)}."
  :mode t)

(defun-sf airy-zero-bi-deriv ((s :unsigned-int))
  "gsl_sf_airy_zero_Bi_deriv_e"
  :return (sf-result)
  :documentation "The location of the @var{s}-th zero of the Airy function derivative @math{Bi'(x)}."
  :mode t)

;;;;****************************************************************************
;;;; Bessel functions
;;;;****************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '(bessel-J0 bessel-J1 bessel-Jn)))

(defun-sf bessel-J0 ((x :double))
  "gsl_sf_bessel_J0_e"
  :documentation
  "The regular cylindrical Bessel function of zeroth order, @math{J_0(x)}."
  :return (sf-result))

(defun-sf bessel-J1 ((x :double))
  "gsl_sf_bessel_J1_e"
  :documentation
  "The regular cylindrical Bessel function of first order, @math{J_1(x)}."
  :return (sf-result))

(defun-sf bessel-Jn ((n :int) (x :double))
  "gsl_sf_bessel_Jn_e"
  :documentation
  "The regular cylindrical Bessel function of order @var{n}, @math{J_n(x)}."
  :return (sf-result))

;;; Doesn't work; how do I get an array of doubles back?
(defun bessel-Jn-array (nmin nmax x)
  "The values of the regular cylindrical Bessel functions @math{J_n(x)} for @math{n} from @var{nmin} to @var{nmax} inclusive.  The values are computed using recurrence relations for efficiency, and therefore may differ slightly from the exact values."
  (with-foreign-objects ((ptr :double))
    (let ((status
	   (foreign-funcall "gsl_sf_bessel_Jn_array"
			    :int
			    nmin
			    :int
			    nmax
			    :double
			    x
			    :pointer
			    ptr
			    :int)))
      (unless
	  (eql :success
	       (foreign-enum-keyword 'gsl-errorno status))
	(warn 'gsl-warning
	      :gsl-errno
	      status
	      :gsl-context
	      `(foobar ,nmin ,nmax ,x)))
      (loop for i from 0 below (- nmax nmin)
	    collect (cffi:mem-aref ptr :double i)))))


(defun-sf regular-spherical-bessel ((order :int) (x :double))
  "gsl_sf_bessel_jl_e"
  :documentation "The regular spherical Bessel function j_l."
  :return (sf-result))

;;; (regular-spherical-bessel 3 12.4d0)
;;; 0.07813007015855176d0
;;; 1.9083196617232666d-16

;;;;****************************************************************************
;;;; Dilogarithm
;;;;****************************************************************************

;;; dilog merge complex and real
(defun dilogarithm (x)
  "The dilogarithm."
  (etypecase x
    (double-float
     (funcall
      (defun-sf :lambda ((x :double)) 
	"gsl_sf_dilog_e"
	:return (sf-result))
      x))
    (complex
     (multiple-value-bind (re re-err im im-err)
	 (funcall
	  ;; returns two gsl_sf_result
	  (defun-sf :lambda
	      ((radius :double) (angle :double))
	    "gsl_sf_complex_dilog_e"
	    :return (sf-result sf-result))
	  (abs x)
	  (phase x))
       (values 
	(complex re im)
	(complex re-err im-err))))))

(defun-sf jacobian-elliptic-functions	  ; CL name
    ((u :double) (m :double))		  ; inputs
  "gsl_sf_elljac_e"			  ; GSL name
  :documentation
  "The Jacobian elliptic functions sn(u|m), cn(u|m), dn(u|m)
   computed by descending Landen transformations.
   See Abramowitz & Stegun, Chapter 16"
  :return (:double :double :double))

;;; > (jacobian-elliptic-functions 0.61802d0 0.5d0)
;;; 0.564575752943391
;;; 0.8253812568676386
;;; 0.916857191493965
;;; > (jacobian-elliptic-functions 0.2d0 0.81d0)
;;; 0.19762082367187697
;;; 0.9802785369736752
;;; 0.9840560289645665
;;; > (jacobian-elliptic-functions 0.61802d0 1.5d0)
;;; ;;;error
