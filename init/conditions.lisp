;; GSL errors                                
;; Liam Healy Sat Mar  4 2006 - 18:33
;; Time-stamp: <2008-03-27 23:07:29EDT conditions.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; GSL conditions
;;;;****************************************************************************

(export 'gsl-condition)
(define-condition gsl-condition (arithmetic-error warning)
  ((error-number :initarg :error-number :reader error-number)
   (error-text :initarg :error-text :reader error-text)   
   (explanation :initarg :explanation :reader explanation)
   (source-file :initform nil :initarg :source-file :reader source-file)
   (line-number :initform 0 :initarg :line-number :reader line-number))
  (:report
   (lambda (condition stream)
     (format stream "~a ~a ~@[in ~a at line ~d~]"
	     (error-text condition)
	     (explanation condition)
	     (source-file condition)
	     (line-number condition))))
  (:documentation
   "A condition that has been signalled by the GNU Scientific Library."))

(defparameter *errorno-keyword* nil)

(defmacro define-gsl-condition (keyword number text)
  `(progn
    (define-condition ,keyword (gsl-condition)
      ((error-number :initform ,number :reader error-number :allocation :class)
       (error-text :initform ,text :reader error-text :allocation :class))
      (:documentation
       (format nil
	       "The condition ~a, ~a, signalled by the GNU Scientific Library."
	       ',keyword ,text)))
    (setf *errorno-keyword* (acons ,number ',keyword *errorno-keyword*))
    (export ',keyword)))

(define-gsl-condition EDOM 1 "Input domain error")
(define-gsl-condition ERANGE 2 "Output range error")
(define-gsl-condition EFAULT 3 "Invalid pointer")
(define-gsl-condition EINVAL 4 "Invalid argument")
(define-gsl-condition EFAILED 5 "Generic failure")
(define-gsl-condition EFACTOR 6 "Factorization failed")
(define-gsl-condition ESANITY 7 "Sanity check failed - shouldn't happen")
(define-gsl-condition ENOMEM 8 "Malloc failed")
(define-gsl-condition EBADFUNC 9 "Problem with user-supplied function")
(define-gsl-condition ERUNAWAY 10 "Iterative process is out of control")
(define-gsl-condition EMAXITER 11 "Exceeded max number of iterations")
(define-gsl-condition EZERODIV 12 "Tried to divide by zero")
(define-gsl-condition EBADTOL 13 "User specified an invalid tolerance")
(define-gsl-condition ETOL 14 "Failed to reach the specified tolerance")
(define-gsl-condition EUNDRFLW 15 "Underflow")
(define-gsl-condition EOVRFLW 16 "Overflow")
(define-gsl-condition ELOSS 17 "Loss of accuracy")
(define-gsl-condition EROUND 18 "Failed because of roundoff error")
(define-gsl-condition EBADLEN 19 "Matrix, vector lengths are not conformant")
(define-gsl-condition ENOTSQR 20 "Matrix not square")
(define-gsl-condition ESING 21 "Apparent singularity detected")
(define-gsl-condition EDIVERGE 22 "Integral or series is divergent")
(define-gsl-condition EUNSUP 23 "Requested feature is not supported by the hardware")
(define-gsl-condition EUNIMPL 24 "Requested feature not (yet) implemented")
(define-gsl-condition ECACHE 25 "Cache limit exceeded")
(define-gsl-condition ETABLE 26 "Table limit exceeded")
(define-gsl-condition ENOPROG 27 "Iteration is not making progress towards solution")
(define-gsl-condition ENOPROGJ 28 "Jacobian evaluations are not improving the solution")
(define-gsl-condition ETOLF 29 "Cannot reach the specified tolerance in F")
(define-gsl-condition ETOLX 30 "Cannot reach the specified tolerance in X")
(define-gsl-condition ETOLG 31 "Cannot reach the specified tolerance in gradient")
(define-gsl-condition EOF 32 "End of file")
;;; It is possible to return +positive-infinity+
;;; by defining a handler for :EOVRFLW.

(defun lookup-condition (number)
  (or (rest (assoc number *errorno-keyword*))
      ;; go for "Generic failure" of the code doesn't come up
      'EFAILED))

(defun signal-gsl-error (number explanation &optional file line)
  "Signal an error from the GSL library."
  (error (lookup-condition number)
	 :explanation explanation
	 :source-file file
	 :line-number line))

(defun signal-gsl-warning (number explanation &optional file line)
  "Signal a warning from the GSL library."
  (warn (lookup-condition number)
	 :explanation explanation
	 :source-file file
	 :line-number line))

(cffi:defcallback gsl-error :void
    ((reason :string) (file :string) (line :int) (error-number :int))
  (signal-gsl-error error-number reason file line))

(defun establish-handler ()
  (cffi:foreign-funcall
   "gsl_set_error_handler"
   :pointer (cffi:callback gsl-error)))

(establish-handler)

;;; This insures that conditions will be signalled if GSLL is dumped
;;; in save-lisp-and-die.
#+sbcl (push 'establish-handler sb-ext:*init-hooks*)

;;;;****************************************************************************
;;;; Define non-error C codes 
;;;;****************************************************************************

(cffi:defcenum gsl-errorno
  "Error codes for GSL, from /usr/include/gsl/gsl_errno.h."
  ;; We really only need the first three here; the rest are handled
  ;; above.
  (:CONTINUE -2)
  :FAILURE :SUCCESS :EDOM :ERANGE :EFAULT :EINVAL :EFAILED :EFACTOR  
  :ESANITY :ENOMEM :EBADFUNC :ERUNAWAY :EMAXITER :EZERODIV :EBADTOL  
  :ETOL :EUNDRFLW :EOVRFLW :ELOSS :EROUND :EBADLEN :ENOTSQR :ESING    
  :EDIVERGE :EUNSUP :EUNIMPL :ECACHE :ETABLE :ENOPROG :ENOPROGJ 
  :ETOLF :ETOLX :ETOLG :EOF)

(defmacro gsl-errorno-sm (keyword)
  `(define-symbol-macro
    ,(intern (symbol-name keyword) (find-package :gsl))
    (cffi:foreign-enum-value 'gsl-errorno ,keyword)))

(gsl-errorno-sm :success)
