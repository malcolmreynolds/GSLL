;; Make tests and examples
;; Liam Healy 2008-09-07 21:00:48EDT generate-tests.lisp
;; Time-stamp: <2008-11-15 13:29:28EST generate-tests.lisp>
;; $Id: $

;;; Througout the GSLL interface definition files are #'save-test
;;; forms.  These serve to define both examples and tests.  Getting an
;;; example involves calling (examples) to get a list of names, then
;;; the calling the function with a particular name to get the
;;; examples, e.g.
;;; (examples)
;;; (examples 'matrix-m+)

;;; To do all the tests,
;;; (lisp-unit:run-tests)

;;; The files that define the tests are in tests/.  These files are
;;; generated automatically and checked into the repository; they
;;; shouldn't be changed very often.  Rarely, it may be necessary to
;;; generate such a file.  In this case, #'write-test-to-file recreates
;;; the file, e.g.
;;; (write-test-to-file 'matrix-m+ "test/")

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

;;; This is needed for debugging, to remove the previous definitions.
(defun delete-test-definition (name)
  (remf *all-generated-tests* name))

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
  '(-34.5d0 8.24d0 3.29d0 -8.93d0 34.12d0 -6.15d0 49.27d0 -13.49d0 32.5d0
    42.73d0 -17.24d0 43.31d0 -16.12d0 -8.25d0 21.44d0 -49.08d0 -39.66d0
    -49.46d0 19.68d0 -5.55d0 -8.82d0 25.37d0 -30.58d0 31.67d0 29.36d0
    -33.24d0 -27.03d0 -41.67d0 42.0d0 -20.81d0 37.93d0 -30.02d0 11.41d0
    22.0d0 33.43d0 42.97d0 -36.7d0 -9.69d0 10.18d0 -47.96d0 37.42d0 -42.55d0
    40.77d0 18.19d0 -43.66d0 -9.3d0 -29.79d0 -38.88d0 -45.6d0 -31.91d0)
  "A sequence of random double floats ranging between -100.0d0 and +100.0d0.")

;;; (loop repeat 50 collect (random 256))
(defparameter *unsigned-byte-pool*
  '(67 44 189 116 163 140 161 215 98 28 10 19 28 178 217 36 109 222 88 97 167
    135 96 198 37 110 12 175 184 62 40 27 146 128 18 237 184 234 24 43 79 49
    156 158 157 167 157 34 219 249)
  "A sequence of random integers ranging between 0 and 255.")

;;; (loop repeat 50 collect (* (if (zerop (random 2)) -1 1) (random 128)))
(defparameter *signed-byte-pool*
  '(-64 -68 71 -91 52 -10 73 -5 123 32 28 30 37 -73 -8 -15 -22 68 -47 -81 -68
    125 -15 -53 -17 -114 24 -60 32 106 -3 37 -1 97 -96 -12 -20 -61 18 108 61
    -82 75 -30 -71 44 48 88 121 106)
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
  (mapcar
   (lambda (num) (coerce num (component-float-type type)))
   (subseq
    (cond ((subtypep type 'unsigned-byte) *unsigned-byte-pool*)
	  ((subtypep type 'signed-byte) *signed-byte-pool*)
	  ((subtypep type 'float) *double-float-pool*)
	  ((subtypep type 'complex) *double-float-pool*))
    starting
    (+ starting (if (subtypep type 'complex) (* 2 length) length)))))


;;;;****************************************************************************
;;;; Generate forms for all array types
;;;;****************************************************************************

(defun array-default (spec &optional no-init sync-on-exit)
  "Make an array of the current type and intialize from the pool."
  (declare (special default-element-type starting-element))
  (let ((matrixp (listp spec)))
    `(,(data-class-name (if matrixp 'matrix 'vector) default-element-type)
       ,(if no-init
	    `',spec		   ; no initial values, just dimension
	    (cons		   ; macro to make initial-contents
	     'a
	     (if matrixp
		 (loop repeat (first spec)
		    collect
		    (make-list-from-pool
		     default-element-type (second spec) starting-element)
		    do
		    (incf starting-element (second spec)))
		 (prog1 
		     (make-list-from-pool
		      default-element-type spec starting-element)
		   (incf starting-element spec)))))
       ,sync-on-exit)))

(defun scalar-default (&optional float-type)
  "Make a scalar of the current type from the pool.  For complex
   types, setting float-type will select a real of the corresponding
   component float type."
  (declare (special default-element-type starting-element))
  (prog1
      (if (subtypep default-element-type 'complex)
	  (if float-type
	      (first
	       (make-list-from-pool
		(component-float-type default-element-type) 1 starting-element))
	      (apply
	       #'complex
	       (make-list-from-pool default-element-type 1 starting-element)))
	  (first
	   (make-list-from-pool default-element-type 1 starting-element)))
    (incf starting-element)))

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
  (loop for det in (element-types element-types)
     collect
     (let ((default-element-type det))
       (declare (special default-element-type starting-element))
       (setf starting-element 0)
       (stupid-code-walk-eval-some
	test
	'(array-default vector-default scalar-default)))))

(defmacro generate-all-array-tests (name element-types test)
  `(save-test ,name ,@(generate-all-array-tests-body element-types test)))

