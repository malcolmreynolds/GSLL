;; GSL errors                                
;; Liam Healy Sat Mar  4 2006 - 18:33
;; Time-stamp: <2008-11-16 14:47:24EST conditions.lisp>
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

#|
;;; This makes the printing of the condition object more informative,
;;; but also overrides the :report which I don't want to do.
(defmethod print-object ((object gsl-condition) stream)
  (print-unreadable-object (object stream :type t :identity t) 
    (format stream "~a (GSL condition)" (error-text object))))
|#

(defparameter *errorno-keyword* nil)

(defmacro define-gsl-condition (keyword number text &rest superclasses)
  `(progn
    (define-condition ,keyword (gsl-condition ,@superclasses)
      ((error-number :initform ,number :reader error-number :allocation :class)
       (error-text :initform ,text :reader error-text :allocation :class))
      (:documentation
       (format nil
	       "The condition ~a, ~a, signalled by the GNU Scientific Library."
	       ',keyword ,text)))
    (setf *errorno-keyword* (acons ,number ',keyword *errorno-keyword*))
    (export ',keyword)))

(define-gsl-condition input-domain 1 "Input domain error")
(define-gsl-condition input-range 2 "Output range error")
(define-gsl-condition invalid-pointer 3 "Invalid pointer")
(define-gsl-condition invalid-argument 4 "Invalid argument")
(define-gsl-condition generic-failure 5 "Generic failure")
(define-gsl-condition factorization-failure 6 "Factorization failed")
(define-gsl-condition sanity-check-failure
    7 "Sanity check failed - shouldn't happen")
(define-gsl-condition memory-allocation-failure 8 "Malloc failed")
(define-gsl-condition bad-function-supplied
    9 "Problem with user-supplied function")
(define-gsl-condition runaway-iteration
    10 "Iterative process is out of control")
(define-gsl-condition exceeded-maximum-iterations
    11 "Exceeded max number of iterations")
(define-gsl-condition gsl-division-by-zero
    12 "Tried to divide by zero" division-by-zero)
(define-gsl-condition invalid-tolerance 13 "User specified an invalid tolerance")
(define-gsl-condition failure-to-reach-tolerance
    14 "Failed to reach the specified tolerance")
(define-gsl-condition underflow 15 "Underflow")
(define-gsl-condition overflow 16 "Overflow")
(define-gsl-condition loss-of-accuracy 17 "Loss of accuracy")
(define-gsl-condition roundoff-failure 18 "Failed because of roundoff error")
(define-gsl-condition nonconformant-dimensions
    19 "Matrix, vector lengths are not conformant")
(define-gsl-condition nonsquare-matrix 20 "Matrix not square")
(define-gsl-condition singularity 21 "Apparent singularity detected")
(define-gsl-condition divergence 22 "Integral or series is divergent")
(define-gsl-condition unsupported-feature
    23 "Requested feature is not supported by the hardware")
(define-gsl-condition unimplemented-feature
    24 "Requested feature not (yet) implemented")
(define-gsl-condition cache-limit-exceeded 25 "Cache limit exceeded")
(define-gsl-condition table-limit-exceeded 26 "Table limit exceeded")
(define-gsl-condition no-progress 27
  "Iteration is not making progress towards solution")
(define-gsl-condition jacobian-not-improving
    28 "Jacobian evaluations are not improving the solution")
(define-gsl-condition failure-to-reach-tolerance-f
    29 "Cannot reach the specified tolerance in F")
(define-gsl-condition failure-to-reach-tolerance-x
    30 "Cannot reach the specified tolerance in X")
(define-gsl-condition failure-to-reach-tolerance-g
    31 "Cannot reach the specified tolerance in gradient")
(define-gsl-condition gsl-eof 32 "End of file" end-of-file)
;;; It is possible to return +positive-infinity+
;;; by defining a handler for 'overflow.

(defun lookup-condition (number)
  (or (rest (assoc number *errorno-keyword*))
      ;; go for "Generic failure" of the code doesn't come up
      'generic-failure))

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
