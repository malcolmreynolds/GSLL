;; GSL errors                                
;; Liam Healy Sat Mar  4 2006 - 18:33
;; Time-stamp: <2008-03-08 12:39:50EST conditions.lisp>
;; $Id$

(in-package :gsl)

(cffi:defcenum gsl-errorno
  "Error codes for GSL, from /usr/include/gsl/gsl_errno.h."
  (:CONTINUE -2)
  :FAILURE :SUCCESS :EDOM :ERANGE :EFAULT :EINVAL :EFAILED :EFACTOR  
  :ESANITY :ENOMEM :EBADFUNC :ERUNAWAY :EMAXITER :EZERODIV :EBADTOL  
  :ETOL :EUNDRFLW :EOVRFLW :ELOSS :EROUND :EBADLEN :ENOTSQR :ESING    
  :EDIVERGE :EUNSUP :EUNIMPL :ECACHE :ETABLE :ENOPROG :ENOPROGJ 
  :ETOLF :ETOLX :ETOLG :EOF)

(defparameter *gsl-error-alist*
  '((-2 . "Iteration has not converged")
    (-1 . "Failure")
    (0 . "Success") 
    (1 . "Input domain error")
    (2 . "Output range error")
    (3 . "Invalid pointer")
    (4 . "Invalid argument")
    (5 . "Generic failure")
    (6 . "Factorization failed")
    (7 . "Sanity check failed - shouldn't happen")
    (8 . "Malloc failed")
    (9 . "Problem with user-supplied function")
    (10 . "Iterative process is out of control")
    (11 . "Exceeded max number of iterations")
    (12 . "Tried to divide by zero")
    (13 . "User specified an invalid tolerance")
    (14 . "Failed to reach the specified tolerance")
    (15 . "Underflow")
    (16 . "Overflow ")
    (17 . "Loss of accuracy")
    (18 . "Failed because of roundoff error")
    (19 . "Matrix, vector lengths are not conformant")
    (20 . "Matrix not square")
    (21 . "Apparent singularity detected")
    (22 . "Integral or series is divergent")
    (23 . "Requested feature is not supported by the hardware")
    (24 . "Requested feature not (yet) implemented")
    (25 . "Cache limit exceeded")
    (26 . "Table limit exceeded")
    (27 . "Iteration is not making progress towards solution")
    (28 . "Jacobian evaluations are not improving the solution")
    (29 . "Cannot reach the specified tolerance in F")
    (30 . "Cannot reach the specified tolerance in X")
    (31 . "Cannot reach the specified tolerance in gradient")
    (32 . "End of file")))

;;; It would be nice to be able to return, or give the option to return,
;;; the portable equivalent of #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
;;; for :EOVRFLW.

(define-condition gsl-error (arithmetic-error)
  ((gsl-errno :initarg :gsl-errno :reader gsl-errno)
   (gsl-reason :initarg :gsl-reason :reader gsl-reason)
   (gsl-source-file :initform nil :initarg :gsl-source-file :reader gsl-source-file)
   (gsl-line-number :initform 0 :initarg :gsl-line-number :reader gsl-line-number))
  (:report
   (lambda (condition stream)
     (format stream "~a (~a), ~a ~@[in ~a at line ~d~]"
	     (rest (assoc (gsl-errno condition) *gsl-error-alist*))
	     (cffi:foreign-enum-keyword 'gsl-errorno (gsl-errno condition))
	     (gsl-reason condition)
	     (gsl-source-file condition)
	     (gsl-line-number condition))))
  (:documentation
   "An error that has been signalled by the GNU Scientific Library."))

(define-condition gsl-warning (warning)
  ((gsl-errno :initarg :gsl-errno :reader gsl-errno)
   (gsl-context :initarg :gsl-context :reader gsl-context))
  (:report
   (lambda (condition stream)
     (format stream "GSL condition ~a (~d), ~a in ~a"
	     (cffi:foreign-enum-keyword 'gsl-errorno (gsl-errno condition))
	     (gsl-errno condition)
	     (rest (assoc (gsl-errno condition) *gsl-error-alist*))
	     (gsl-context condition))))
  (:documentation
   "An warning that has been signalled by the GNU Scientific Library."))

(cffi:defcallback gsl-error :void
    ((reason :string) (file :string) (line :int) (error-number :int))
  (error 'gsl-error
	 :gsl-errno error-number
	 :gsl-reason reason
	 :gsl-source-file file
	 :gsl-line-number line))

(defun establish-handler ()
  (cffi:foreign-funcall
   "gsl_set_error_handler"
   :pointer (cffi:callback gsl-error)))

(establish-handler)

;;; This insures that conditions will be signalled if GSLL is dumped
;;; in save-lisp-and-die.
#+sbcl (push 'establish-handler sb-ext:*init-hooks*)

(defmacro gsl-errorno-sm (keyword)
  `(define-symbol-macro
    ,(intern (symbol-name keyword) (find-package :gsl))
    (cffi:foreign-enum-value 'gsl-errorno ,keyword)))

(gsl-errorno-sm :success)
