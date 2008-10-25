;; Make tests and examples
;; Liam Healy 2008-09-07 21:00:48EDT generate-tests.lisp
;; Time-stamp: <2008-10-25 19:06:40EDT generate-tests.lisp>
;; $Id: $

(in-package :gsl)

(export 'examples)

;;;;****************************************************************************
;;;; Data pool 
;;;;****************************************************************************

(defun numerical-serialize (form)
  (if (typep form 'list)
      (cons 'list (mapcar #'numerical-serialize form))
      form))

;;; (make-test '(legendre-conicalP-half 3.5d0 10.0d0))
(defun make-test (form)
  "Make a test for lisp-unit."
  (let ((vals (multiple-value-list (ignore-errors (eval form)))))
    (if (typep (second vals) 'condition)
	`(lisp-unit::assert-error
	  ',(type-of (second vals))
	  ,form)
	`(lisp-unit::assert-numerical-equal
	  ,(numerical-serialize vals)
	  (multiple-value-list ,form)))))

(defparameter *all-generated-tests* nil) 

(defmacro save-test (name &rest forms)
  "Save the test with the given name."
  `(setf
    (getf *all-generated-tests* ',name)
    (remove-duplicates
     (append
      ',forms
      (getf *all-generated-tests* ',name))
     :test #'equal)))

(defun examples (&optional name)
  "If no argument is supplied, list the names of the example categories.
   If a category name is given as the argument, give the list of examples
   in that category."
  (if name 
      (getf *all-generated-tests* name)
      (loop for (key value) on *all-generated-tests* by #'cddr
	 collect key)))

(defun create-test (test-name)
  "Find the saved test by name and create it, with the generated results."
  (append
   `(lisp-unit:define-test ,test-name)
   (mapcar #'make-test (getf *all-generated-tests* test-name))))

(defun write-test-to-file (test path)
  "Write the test to a file with the same name under path.
   Use this function with caution; it will replace an existing
   test file and thus the opportunity for regression test will be lost."
  (let ((pathname (merge-pathnames (format nil "~(~a~).lisp" test) path))
	(*read-default-float-format* 'single-float))
    (with-open-file
	(stream pathname :direction :output :if-exists :rename)
      (format
       stream
       ";; Regression test ~a for GSLL, automatically generated~%~%" test)
      (format stream "(in-package :gsl)~%~%")
      (format t "Writing test ~a to file ~a~&" test pathname)
      (format stream "~s~%~%" (create-test test)))))


;;; This is commented out because it shouldn't be run.  It will
;;; regenerate all tests, so there will be no regression tests to
;;; previous versions.
#+(or)
(defun write-tests (path)
  "Write all the tests to the appropriate file."
  (iter
    (for (key val) on *all-generated-tests* by #'cddr)
    (write-test-to-file key path)))

#|
;;; Obsolete; to put all tests in one file.
(defun write-tests-to-file (filename)
  "Write all the saved tests with expected results to the file."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream ";; Regression tests for GSLL, automatically generated~%~%")
    (format stream "(in-package :gsl)~%~%")
    (loop for (test-name forms) on *all-generated-tests* by #'cddr
       do
       (format t "Writing test ~a to file~&" test-name)
       (format stream "~s~%~%" (create-test test-name)))))

|#

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
		    (stupid-code-walk-eval-some se eval-list))
		  form))))

(defun generate-all-array-tests-body (element-types test)
  (iter (for default-element-type in (element-types element-types))
	(declare (special default-element-type starting-element))
	(setf starting-element 0)
	(collect
	    (stupid-code-walk-eval-some
	     test
	     '(vector-default scalar-default)))))

(defmacro generate-all-array-tests (name element-types test)
  `(save-test ,name ,@(generate-all-array-tests-body element-types test)))

