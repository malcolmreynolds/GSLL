;; CFFI-Grovel definitions for unix systems.
;; Liam Healy 2009-05-25 13:10:50EDT libgsl-unix.lisp
;; Time-stamp: <2009-08-23 10:07:35EDT libgsl-unix.lisp>
;; $Id: $

(in-package :gsl)

#+linux
(define "_GNU_SOURCE")

;;; When installed through Mac Ports, GSL .h files will be found
;;; in /opt/local/include.
#+darwin
(cc-flags "-I/opt/local/include/")

(include "gsl/gsl_integration.h")

;; gsl_integration_qawo_enum
(cenum integrate-sine-cosine
       ((:cosine "GSL_INTEG_COSINE"))
       ((:sine "GSL_INTEG_SINE")))
 
(cenum integrate-method
       ((:gauss15 "GSL_INTEG_GAUSS15"))
       ((:gauss21 "GSL_INTEG_GAUSS21"))
       ((:gauss31 "GSL_INTEG_GAUSS31"))
       ((:gauss41 "GSL_INTEG_GAUSS41"))
       ((:gauss51 "GSL_INTEG_GAUSS51"))
       ((:gauss61 "GSL_INTEG_GAUSS61")))

(include "gsl/gsl_eigen.h")

;; gsl_eigen_sort_t
(cenum eigen-sort-type
       ((:value-ascending "GSL_EIGEN_SORT_VAL_ASC"))
       ((:value-descending "GSL_EIGEN_SORT_VAL_DESC"))
       ((:absolute-ascending "GSL_EIGEN_SORT_ABS_ASC"))
       ((:absolute-descending "GSL_EIGEN_SORT_ABS_DESC")))

(include "gsl/gsl_cblas.h")

(cenum cblas-transpose
       ;; CBLAS_TRANSPOSE
       ((:notrans "CblasNoTrans"))
       ((:trans "CblasTrans"))
       ((:conjtrans "CblasConjTrans")))

(cenum cblas-uplo
       ;; CBLAS_UPLO
       ((:upper "CblasUpper"))
       ((:lower "CblasLower")))

(cenum cblas-diag
       ;; CBLAS_DIAG
       ((:nonunit "CblasNonUnit"))
       ((:unit "CblasUnit")))

(cenum cblas-side
       ;; CBLAS_SIDE
       ((:left "CblasLeft"))
       ((:right "CblasRight")))

(include "gsl/gsl_odeiv.h")

(cenum step-size-adjustment
       ((:step-size-decreased "GSL_ODEIV_HADJ_DEC"))
       ((:step-size-unchanged "GSL_ODEIV_HADJ_NIL"))
       ((:step-size-increased "GSL_ODEIV_HADJ_INC")))

(include "gsl/gsl_ieee_utils.h")

(cenum ieee-types
       ((:NAN "GSL_IEEE_TYPE_NAN"))
       ((:INF "GSL_IEEE_TYPE_INF"))
       ((:NORMAL "GSL_IEEE_TYPE_NORMAL"))
       ((:DENORMAL "GSL_IEEE_TYPE_DENORMAL"))
       ((:ZERO "GSL_IEEE_TYPE_ZERO")))

(cenum ieee-precisions
       ((:SINGLE-PRECISION "GSL_IEEE_SINGLE_PRECISION"))
       ((:DOUBLE-PRECISION "GSL_IEEE_DOUBLE_PRECISION"))
       ((:EXTENDED-PRECISION "GSL_IEEE_EXTENDED_PRECISION")))

(cenum ieee-rounding
       ((:TO-NEAREST "GSL_IEEE_ROUND_TO_NEAREST"))
       ((:DOWN "GSL_IEEE_ROUND_DOWN"))
       ((:UP "GSL_IEEE_ROUND_UP"))
       ((:TO-ZERO "GSL_IEEE_ROUND_TO_ZERO")))

(cenum ieee-mask
       ((:INVALID "GSL_IEEE_MASK_INVALID"))
       ((:DENORMALIZED "GSL_IEEE_MASK_DENORMALIZED"))
       ((:DIVISION-BY-ZERO "GSL_IEEE_MASK_DIVISION_BY_ZERO"))
       ((:OVERFLOW "GSL_IEEE_MASK_OVERFLOW"))
       ((:UNDERFLOW "GSL_IEEE_MASK_UNDERFLOW"))
       ((:ALL "GSL_IEEE_MASK_ALL"))
       ((:INEXACT "GSL_IEEE_TRAP_INEXACT")))

(include "gsl/gsl_mode.h")

(cenum sf-mode
       ((:double "GSL_PREC_DOUBLE"))
       ((:single "GSL_PREC_SINGLE"))
       ((:approx "GSL_PREC_APPROX")))

(include "gsl/gsl_errno.h")

(cenum gsl-errorno
    ((:CONTINUE "GSL_CONTINUE"))
    ((:FAILURE "GSL_FAILURE"))
    ((:SUCCESS "GSL_SUCCESS"))
    ((:EDOM "GSL_EDOM"))
    ((:ERANGE "GSL_ERANGE"))
    ((:EFAULT "GSL_EFAULT"))
    ((:EINVAL "GSL_EINVAL"))
    ((:EFAILED "GSL_EFAILED"))
    ((:EFACTOR "GSL_EFACTOR"))
    ((:ESANITY "GSL_ESANITY"))
    ((:ENOMEM "GSL_ENOMEM"))
    ((:EBADFUNC "GSL_EBADFUNC"))
    ((:ERUNAWAY "GSL_ERUNAWAY"))
    ((:EMAXITER "GSL_EMAXITER"))
    ((:EZERODIV "GSL_EZERODIV"))
    ((:EBADTOL "GSL_EBADTOL"))  
    ((:ETOL "GSL_ETOL"))
    ((:EUNDRFLW "GSL_EUNDRFLW"))
    ((:EOVRFLW "GSL_EOVRFLW"))
    ((:ELOSS "GSL_ELOSS"))
    ((:EROUND "GSL_EROUND"))
    ((:EBADLEN "GSL_EBADLEN"))
    ((:ENOTSQR "GSL_ENOTSQR"))
    ((:ESING "GSL_ESING"))    
    ((:EDIVERGE "GSL_EDIVERGE"))
    ((:EUNSUP "GSL_EUNSUP"))
    ((:EUNIMPL "GSL_EUNIMPL"))
    ((:ECACHE "GSL_ECACHE"))
    ((:ETABLE "GSL_ETABLE"))
    ((:ENOPROG "GSL_ENOPROG"))
    ((:ENOPROGJ "GSL_ENOPROGJ")) 
    ((:ETOLF "GSL_ETOLF"))
    ((:ETOLX "GSL_ETOLX"))
    ((:ETOLG "GSL_ETOLG"))
    ((:EOF "GSL_EOF")))

(ctype sizet "size_t")
