;; Make tests and examples
;; Liam Healy 2008-09-07 21:00:48EDT generate-tests.lisp
;; Time-stamp: <2008-09-14 18:14:17EDT generate-tests.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Data pool 
;;;;****************************************************************************

;; Signed
;;(loop repeat 100 collect (* (if (zerop (random 2)) -1 1) (random 128)))
;; Unsigned
;;(loop repeat 100 collect (random 128))
;;(loop repeat 100 collect (random 128.0d0))
;; Make short-decimal floats for floats?

;;; (loop repeat 50 collect (coerce (- (/ (random 10000) 100) 50) 'double-float))
(defparameter *double-float-pool*
  '(-37.61 -48.84 -48.86 29.32 2.43 10.83 20.93 -15.27 11.26 -9.16 38.5 25.53
    -25.72 -13.99 -10.95 -0.71 36.53 -24.44 -21.79 -17.01 11.59 -37.64 -20.97
    -44.42 42.09 -46.72 13.32 -37.18 25.98 -21.06 40.78 21.35 43.15 9.23
    31.73 33.38 -48.95 -42.69 22.95 -47.97 37.37 3.01 -3.53 -38.5 -12.8 42.14
    -6.44 29.57 -34.1 -11.98)
  "A sequence of random double floats ranging between -100.0d0 and +100.0d0.")

;;; (loop repeat 50 collect (random 256))
(defparameter *unsigned-byte-pool*
  '(67 44 189 116 163 140 161 215 98 28 10 19 28 178 217 36 109 222 88 97 167
    135 96 198 37 110 12 175 184 62 40 27 146 128 18 237 184 234 24 43 79 49
    156 158 157 167 157 34 219 249)
  "A sequence of random integers ranging between 0 and 255.")

;;; (loop repeat 50 collect (* (if (zerop (random 2)) -1 1) (random 256)))
(defparameter *signed-byte-pool*
  '(-161 17 -194 -135 -90 253 -22 22 -102 -27 -254 175 -107 -76 108 -209 54
    -239 -73 237 -80 -92 150 -107 164 63 -48 -142 0 105 182 253 96 124 -28
    -153 -84 188 181 3 96 -114 -222 -39 225 233 84 -218 51 -188)
  "A sequence of random integers ranging between -255 and 255.")

(defun make-vector-from-pool (type length &optional (starting 0))
  "Make a vector of the specified element type and length using the
   pool data for the type and starting at the specified point in the pool."
  (make-array*
   length type
   :initial-contents
   (make-list-from-pool type length starting)))

(defun make-list-from-pool (type length &optional (starting 0))
  "Make a list for :initial-contents of the specified element type and
   length using the pool data for the type and starting at the
   specified point in the pool."
  (subseq
   (cond ((subtypep type 'unsigned-byte) *unsigned-byte-pool*)
	 ((subtypep type 'signed-byte) *signed-byte-pool*)
	 ((subtypep type 'float) *double-float-pool*)
	 ((subtypep type 'complex) *double-float-pool*))
   starting
   (+ starting (if (subtypep type 'complex) (* 2 length) length))))


;;;;****************************************************************************
;;;; Generate forms for all array types
;;;;****************************************************************************

(defun vector-default (spec &optional no-init sync-on-exit)
  (declare (special default-element-type starting-element))
  (prog1
      `(,(data-class-name 'vector default-element-type)
	 ,(if no-init
	      spec		   ; no initial values, just dimension
	      (cons		   ; macro to make initial-contents
	       'a
	       (make-list-from-pool default-element-type spec starting-element)))
	 ,sync-on-exit)
    (incf starting-element spec)))

(defun scalar-default (spec)
  (declare (special default-element-type))
  (coerce spec default-element-type))

(defun stupid-code-walk-eval-some (form eval-list)
  "Walk the form and if the first symbol of the list is a member of
   eval-list, evaluate it and use the result.  Otherwise use the result
   as-is."
  (if (atom form)
      form
      (if (member (first form) eval-list)
	  (eval form)
	  (mapcar (lambda (se)
		    (stupid-walk-form-eval-some se eval-list))
		  form))))

(defun generate-all-array-tests (element-types test)
  (iter (for default-element-type in (element-types element-types))
	(declare (special default-element-type starting-element))
	(setf starting-element 0)
	(collect
	    (stupid-code-walk-eval-some
	     test
	     '(vector-default scalar-default)))))



;;;;;;;;;;

;;; doesn't work for complex because:
;;; Cannot pass complex scalars to and from GSL functions (structs passed by value).
(generate-all-array-tests :no-complex
 '(letm ((v1 (vector-default 3 t))
	 (v2 (vector-default 3)))
   (set-all v1 (scalar-default 2))
   (m+ v1 v2))))


;;; doesn't work for complex because GSL 
(generate-all-array-tests :no-complex
 '(letm ((v1 (vector-default 3))
	 (v2 (vector-default 3)))
   (m+ v1 v2)))
